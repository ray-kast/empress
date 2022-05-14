#![deny(
    clippy::suspicious,
    clippy::style,
    rustdoc::broken_intra_doc_links,
    missing_debug_implementations,
    missing_copy_implementations
)]
#![warn(clippy::pedantic, clippy::cargo, missing_docs)]

//! Binary crate for `empress`.  See [the
//! README](https://github.com/ray-kast/empress/blob/master/README.md) for more
//! details.

use std::{
    fmt,
    fmt::{Display, Formatter},
};

use anyhow::{anyhow, Context, Error};
use clap::Parser;
use lazy_static::lazy_static;
use log::{error, LevelFilter};
use tokio::runtime::Builder as RtBuilder;

mod client;
mod format;
mod interface;
mod server;

type Result<T = (), E = Error> = std::result::Result<T, E>;

#[derive(Parser)]
enum Opts {
    /// Launch a D-Bus service abstracting MPRIS players
    Server {
        /// Disable info logs (enabled by default if stderr is not a TTY)
        #[clap(short, long)]
        quiet: bool,

        /// Enable info logs, even if stderr is not a TTY
        #[clap(long, conflicts_with("quiet"))]
        no_quiet: bool,

        /// Output extra information to the console
        #[clap(
            short,
            long,
            parse(from_occurrences),
            conflicts_with("quiet"),
            conflicts_with("no-quiet")
        )]
        verbose: usize,
    },
    #[clap(flatten)]
    Client(ClientCommand),
}

#[derive(Debug, Clone, Parser)]
enum ClientCommand {
    /// List the players currently tracked by the daemon
    ListPlayers,
    /// Skip one track forwards
    Next(PlayerSearchOpts),
    /// Print information about the current track
    #[clap(long_about = include_str!("../etc/now_playing_long.txt"))]
    NowPlaying {
        #[clap(flatten)]
        search: PlayerSearchOpts,

        /// Instead of outputting JSON, output a plaintext string with the given
        /// format.  See the full help for this command for a syntax reference.
        #[clap(short, long)]
        format: Option<String>,
    },
    /// Skip one track backwards
    Previous(PlayerSearchOpts),
    /// Pause a currently-playing player
    Pause(PlayerSearchOpts),
    /// Like pause if a player is playing, otherwise like play
    PlayPause(PlayerSearchOpts),
    /// Stop a currently-playing player
    Stop(PlayerSearchOpts),
    /// Play a currently-paused player
    Play(PlayerSearchOpts),
    /// Seek to a position on a player
    Seek {
        #[clap(flatten)]
        search: PlayerSearchOpts,

        /// The position to seek to, either absolute (e.g. 5) or relative (e.g.
        /// 5+ or 5-)
        pos: Offset,
    },
    /// Get or set the volume on a player
    Volume {
        #[clap(flatten)]
        search: PlayerSearchOpts,

        /// The volume as a number between 0.0 and 1.0, either absolute (e.g.
        /// 0.5) or relative (e.g. 0.1+ or 0.1-).  If no value is given
        /// the current volume is simply printed instead.
        #[clap(default_value_t = Offset::Relative(0.0))]
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
        #[clap(short, long)]
        no_play: bool,
    },
}

#[derive(Debug, Clone, Parser)]
struct PlayerSearchOpts {}

impl From<PlayerSearchOpts> for interface::PlayerSearchOpts {
    fn from(PlayerSearchOpts {}: PlayerSearchOpts) -> Self { Self {} }
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
        write!(f, "{}", offs)?;

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
            let time: f64 = caps[1].parse().context("invalid number for offset")?;

            Ok(match caps.get(2).map(|c| c.as_str()) {
                Some("-") => Offset::Relative(-time),
                Some("+") => Offset::Relative(time),
                Some(_) => unreachable!(),
                None => Offset::Absolute(time),
            })
        } else {
            Err(anyhow!("invalid format for seek position"))
        }
    }
}

fn log_init(v: usize, f: impl FnOnce(&mut env_logger::Builder)) -> Result {
    const VERBOSITY: [LevelFilter; 3] = [LevelFilter::Info, LevelFilter::Debug, LevelFilter::Trace];
    #[cfg(debug_assertions)]
    const DEFAULT_V: usize = 1;
    #[cfg(not(debug_assertions))]
    const DEFAULT_V: usize = 0;

    let mut b = env_logger::builder();

    b.filter_level(VERBOSITY[(DEFAULT_V + v).min(VERBOSITY.len() - 1)]);

    f(&mut b);

    b.parse_default_env().try_init().map_err(Into::into)
}

fn main() {
    let result = run();

    match result {
        Ok(()) => (),
        Err(e) => {
            log_init(0, |_| ()).ok();

            error!("{:?}", e);
        },
    }
}

fn run() -> Result {
    let rt = RtBuilder::new_current_thread()
        .enable_all()
        .build()
        .context("failed to start runtime")?;

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
