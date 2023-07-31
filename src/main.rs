#![deny(
    clippy::disallowed_methods,
    clippy::suspicious,
    clippy::style,
    clippy::clone_on_ref_ptr,
    rustdoc::broken_intra_doc_links,
    missing_debug_implementations,
    missing_copy_implementations
)]
#![warn(clippy::pedantic, clippy::cargo, missing_docs)]

//! Binary crate for `empress`.  See [the
//! README](https://github.com/ray-kast/empress/blob/master/README.md) for more
//! details.

// TODO: implement PlayerOpts
// TODO: add a refresh subcommand
//        - add warnings for detected sync errors during refresh
// TODO: convert now-playing to a property

use std::{
    fmt,
    fmt::{Display, Formatter},
};

use anyhow::{anyhow, Context, Error};
use clap::Parser;
use lazy_static::lazy_static;
use log::{error, LevelFilter};
use server::mpris::player::PlaybackStatus;
use tokio::runtime::Builder as RtBuilder;
use zbus::{
    names::{OwnedInterfaceName, OwnedWellKnownName},
    zvariant::{self, OwnedObjectPath},
};

mod client;
mod format;
mod server;
mod timeout;

type Result<T = (), E = Error> = std::result::Result<T, E>;

#[cfg(debug_assertions)]
lazy_static! {
    static ref NAME_PREFIX: String = format!("net.ryan_s.debug.{}", *API_IDENT);
}

#[cfg(not(debug_assertions))]
lazy_static! {
    static ref NAME_PREFIX: String = format!("net.ryan_s.{}", *API_IDENT);
}

lazy_static! {
    static ref PATH_PREFIX: String = format!("/net/ryan_s/{}", *API_IDENT);

    static ref API_IDENT: String = format!("Empress{}", env!("CARGO_PKG_VERSION_MAJOR"));
    // Interface name is non-negotiable, so don't add a .debug prefix
    static ref INTERFACE_NAME: OwnedInterfaceName = format!("net.ryan_s.{}.Daemon", *API_IDENT)
        .try_into()
        .unwrap();
    static ref SERVER_NAME: OwnedWellKnownName = NAME_PREFIX.as_str().try_into().unwrap();
    static ref SERVER_PATH: OwnedObjectPath =
        format!("{}/Daemon", *PATH_PREFIX).try_into().unwrap();
}

#[derive(Parser)]
#[command(author, about, version)]
enum Opts {
    /// Launch a D-Bus service abstracting MPRIS players
    Server {
        /// Disable info logs (enabled by default if stderr is not a TTY)
        #[arg(short, long)]
        quiet: bool,

        /// Enable info logs, even if stderr is not a TTY
        #[arg(long, conflicts_with("quiet"))]
        no_quiet: bool,

        /// Output extra information to the console
        #[arg(
            short,
            long,
            action(clap::ArgAction::Count),
            conflicts_with("quiet"),
            conflicts_with("no_quiet")
        )]
        verbose: u8,
    },
    #[command(flatten)]
    Client(ClientCommand),
}

#[derive(Debug, Clone, clap::Subcommand)]
enum ClientCommand {
    /// List the players currently tracked by the daemon
    ListPlayers,
    /// Skip one track forwards
    Next(PlayerOpts),
    /// Print information about the current track
    #[command(long_about = include_str!("../etc/now_playing_long.txt"))]
    NowPlaying {
        #[command(flatten)]
        player: PlayerOpts,

        /// Instead of outputting JSON, output a plaintext string with the given
        /// format.  See the full help for this command for a syntax reference.
        #[arg(short, long)]
        format: Option<String>,
    },
    /// Skip one track backwards
    Previous(PlayerOpts),
    /// Pause a currently-playing player
    Pause(PlayerOpts),
    /// Like pause if a player is playing, otherwise like play
    PlayPause(PlayerOpts),
    /// Stop a currently-playing player
    Stop(PlayerOpts),
    /// Play a currently-paused player
    Play(PlayerOpts),
    /// Seek to a position on a player
    Seek {
        #[command(flatten)]
        player: PlayerOpts,

        /// The position to seek to, either absolute (e.g. 5) or relative (e.g.
        /// 5+ or 5-)
        to: Offset,
    },
    /// Get or set the volume on a player
    Volume {
        #[command(flatten)]
        player: PlayerOpts,

        /// The volume as a number between 0.0 and 1.0, either absolute (e.g.
        /// 0.5) or relative (e.g. 0.1+ or 0.1-).  If no value is given
        /// the current volume is simply printed instead.
        #[arg(default_value_t = Offset::Relative(0.0))]
        vol: Offset,
    },
    /// Bump the priority of a specific player
    ///
    /// Note that if --no-play is passed, any players with a status of Playing
    /// will still hold priority over the selected player.
    SwitchCurrent {
        /// The player ID to switch to.  For a list of valid players see the
        /// list-players subcommand.
        to: String,

        /// By default switch-current will pause any currently running players
        /// and play the selected player.  Pass this flag to disable
        /// this behavior.
        #[arg(short, long)]
        no_play: bool,
    },
}

/// Options for filtering the search set of players for the daemon
#[derive(
    Debug, Clone, clap::Args, zvariant::SerializeDict, zvariant::DeserializeDict, zvariant::Type,
)]
#[zvariant(signature = "dict")]
pub struct PlayerOpts {
    #[arg(long, short)]
    name: Option<String>,

    #[arg(long, use_value_delimiter(true))]
    state: Vec<PlaybackStatus>,
}

#[derive(Debug, Clone, Copy)]
enum Offset {
    Relative(f64),
    Absolute(f64),
}

impl Offset {
    fn offset(&self) -> f64 {
        *match self {
            Self::Relative(o) | Self::Absolute(o) => o,
        }
    }
}

impl Display for Offset {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let offs = self.offset();
        assert!(offs.is_sign_positive());
        write!(f, "{offs}")?;

        if matches!(self, Self::Relative(..)) {
            f.write_str(if offs.is_sign_negative() { "-" } else { "+" })?;
        }

        Ok(())
    }
}

impl ::std::str::FromStr for Offset {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use regex::Regex;

        lazy_static! {
            static ref SEEK_PATTERN: Regex = Regex::new(r"(\d*(?:\.\d+)?)(\+|-)?").unwrap();
        }

        if let Some(caps) = SEEK_PATTERN.captures(s) {
            let time: f64 = caps[1].parse().context("Invalid number for offset")?;

            Ok(match caps.get(2).map(|c| c.as_str()) {
                Some("-") => Offset::Relative(-time),
                Some("+") => Offset::Relative(time),
                Some(_) => unreachable!(),
                None => Offset::Absolute(time),
            })
        } else {
            Err(anyhow!("Invalid format for seek position"))
        }
    }
}

fn log_init(v: u8, f: impl FnOnce(&mut env_logger::Builder)) -> Result {
    const VERBOSITY: [LevelFilter; 3] = [LevelFilter::Info, LevelFilter::Debug, LevelFilter::Trace];
    #[cfg(debug_assertions)]
    const DEFAULT_V: u8 = 1;
    #[cfg(not(debug_assertions))]
    const DEFAULT_V: u8 = 0;

    let mut b = env_logger::builder();

    b.filter_level(
        VERBOSITY[usize::from(DEFAULT_V.saturating_add(v)).min(VERBOSITY.len().saturating_sub(1))],
    );

    f(&mut b);

    b.parse_default_env().try_init().map_err(Into::into)
}

fn main() {
    let result = run();

    match result {
        Ok(()) => (),
        Err(e) => {
            log_init(0, |_| ()).ok();

            error!("{e:?}");
        },
    }
}

fn run() -> Result {
    let rt = RtBuilder::new_current_thread()
        .enable_all()
        .build()
        .context("Error starting async runtime")?;

    let opts = Opts::parse();

    match opts {
        Opts::Server {
            quiet,
            no_quiet,
            verbose,
        } => {
            log_init(verbose, |b| {
                if !(no_quiet || verbose != 0 || atty::is(atty::Stream::Stderr)) || quiet {
                    b.filter_level(LevelFilter::Warn);
                }
            })
            .unwrap();

            rt.block_on(server::run())
        },
        Opts::Client(id) => {
            log_init(0, |_| ()).unwrap();

            rt.block_on(client::run(id))
        },
    }
}
