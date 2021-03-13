#![warn(missing_docs, clippy::all, clippy::pedantic, clippy::cargo)]
#![deny(missing_debug_implementations)]

//! Binary crate for `empress`.  See [the
//! README](https://github.com/ray-kast/empress/blob/master/README.md) for more
//! details.

use std::{
    fmt,
    fmt::{Display, Formatter},
};

use anyhow::{anyhow, Context, Error};
use lazy_static::lazy_static;
use log::{error, LevelFilter};
use structopt::StructOpt;
use tokio::runtime::Builder as RtBuilder;

mod client;
mod server;

type Result<T = (), E = Error> = std::result::Result<T, E>;

#[cfg(debug_assertions)]
lazy_static! {
    static ref NAME_PREFIX: String = format!("net.ryan_s.debug.{}", *API_IDENT);
    static ref PATH_PREFIX: String = format!("/net/ryan_s/debug/{}", *API_IDENT);
}

#[cfg(not(debug_assertions))]
lazy_static! {
    static ref NAME_PREFIX: String = format!("net.ryan_s.{}", *API_IDENT);
    static ref PATH_PREFIX: String = format!("/net/ryan_s/{}", *API_IDENT);
}

lazy_static! {
    static ref API_IDENT: String = format!("Empress{}", env!("CARGO_PKG_VERSION_MAJOR"));
    static ref INTERFACE_NAME: String = format!("{}.Daemon", *NAME_PREFIX);
    static ref SERVER_NAME: String = NAME_PREFIX.clone();
    static ref SERVER_PATH: String = format!("{}/Daemon", *PATH_PREFIX);
}

#[derive(Debug, Clone, Copy)]
enum MethodId {
    /// List the players currently tracked by the daemon
    ListPlayers,
    /// Skip one track forwards
    Next,
    /// Skip one track backwards
    Previous,
    /// Pause a currently-playing player
    Pause,
    /// Like pause if a player is playing, otherwise like play
    PlayPause,
    /// Stop a currently-playing player
    Stop,
    /// Play a currently-paused player
    Play,
    /// Seek to a relative position on a player
    SeekRelative,
    /// Seek to an absolute position on a player
    SeekAbsolute,
}

impl Display for MethodId {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::ListPlayers => "ListPlayers",
            Self::Next => "Next",
            Self::Previous => "Previous",
            Self::Pause => "Pause",
            Self::PlayPause => "PlayPause",
            Self::Stop => "Stop",
            Self::Play => "Play",
            Self::SeekRelative => "SeekRelative",
            Self::SeekAbsolute => "SeekAbsolute",
        })
    }
}

#[derive(StructOpt)]
enum Opts {
    /// Launch a D-Bus service abstracting MPRIS players
    Server {
        /// Disable info logs (enabled by default if stderr is not a TTY)
        #[structopt(short, long)]
        quiet: bool,

        /// Enable info logs, even if stderr is not a TTY
        #[structopt(long, conflicts_with("quiet"))]
        no_quiet: bool,

        /// Output extra information to the console
        #[structopt(
            short,
            long,
            parse(from_occurrences),
            conflicts_with("quiet"),
            conflicts_with("no-quiet")
        )]
        verbose: usize,
    },
    #[structopt(flatten)]
    Client(ClientCommand),
}

#[derive(Debug, Clone, StructOpt)]
enum ClientCommand {
    /// List the players currently tracked by the daemon
    ListPlayers,
    /// Skip one track forwards
    Next(PlayerOpts),
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
        #[structopt(flatten)]
        player: PlayerOpts,

        /// The position to seek to, either absolute (e.g. 5) or relative (e.g.
        /// +5 or -5)
        to: SeekPosition,
    },
}

impl ClientCommand {
    pub fn id(&self) -> MethodId {
        match self {
            Self::ListPlayers => MethodId::ListPlayers,
            Self::Next(..) => MethodId::Next,
            Self::Previous(..) => MethodId::Previous,
            Self::Pause(..) => MethodId::Pause,
            Self::PlayPause(..) => MethodId::PlayPause,
            Self::Stop(..) => MethodId::Stop,
            Self::Play(..) => MethodId::Play,
            Self::Seek {
                to: SeekPosition::Relative(..),
                ..
            } => MethodId::SeekRelative,
            Self::Seek {
                to: SeekPosition::Absolute(..),
                ..
            } => MethodId::SeekAbsolute,
        }
    }
}

#[derive(Debug, Clone, StructOpt)]
struct PlayerOpts {} // WARNING: DO NOT TOUCH WITHOUT INCREMENTING MAJOR VERSION

#[derive(Debug, Clone, Copy)]
enum SeekPosition {
    Relative(f64),
    Absolute(f64),
}

impl SeekPosition {
    fn pos(&self) -> f64 {
        *match self {
            Self::Relative(p) | Self::Absolute(p) => p,
        }
    }
}

impl ::std::str::FromStr for SeekPosition {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use regex::Regex;

        lazy_static! {
            static ref SEEK_PATTERN: Regex = Regex::new(r"(\+|-)?(\d+(?:\.\d+)?)").unwrap();
        }

        if let Some(caps) = SEEK_PATTERN.captures(s) {
            let time: f64 = caps[2]
                .parse()
                .context("invalid number for seek position")?;

            Ok(match caps.get(1).map(|c| c.as_str()) {
                Some("-") => SeekPosition::Relative(-time),
                Some("+") => SeekPosition::Relative(time),
                Some(_) => unreachable!(),
                None => SeekPosition::Absolute(time),
            })
        } else {
            Err(anyhow!("invalid format for seek position"))
        }
    }
}

fn log_cfg(v: usize) -> env_logger::Builder {
    const VERBOSITY: [LevelFilter; 3] = [LevelFilter::Info, LevelFilter::Debug, LevelFilter::Trace];
    #[cfg(debug_assertions)]
    const DEFAULT_V: usize = 1;
    #[cfg(not(debug_assertions))]
    const DEFAULT_V: usize = 0;

    let mut b = env_logger::builder();

    b.filter_level(VERBOSITY[(DEFAULT_V + v).min(VERBOSITY.len() - 1)]);
    b
}

fn main() {
    let result = run();

    log_cfg(0).try_init().ok();

    match result {
        Ok(()) => (),
        Err(e) => error!("{:?}", e),
    }
}

fn run() -> Result {
    let rt = RtBuilder::new_current_thread()
        .enable_all()
        .build()
        .context("failed to start runtime")?;

    let opts = Opts::from_args();

    match opts {
        Opts::Server {
            quiet,
            no_quiet,
            verbose,
        } => {
            let mut log_cfg = log_cfg(verbose);

            if !(no_quiet || verbose != 0 || atty::is(atty::Stream::Stderr)) || quiet {
                log_cfg.filter_level(LevelFilter::Warn);
            }

            log_cfg.init();

            rt.block_on(server::run())
        },
        Opts::Client(id) => {
            log_cfg(0).init();

            rt.block_on(client::run(id))
        },
    }
}
