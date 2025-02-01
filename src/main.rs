//! Binary crate for `empress`.  See [the
//! README](https://github.com/ray-kast/empress/blob/master/README.md) for more
//! details.

use std::sync::LazyLock;

use anyhow::{Context, Error};
use clap::Parser;
use log::error;
use tokio::runtime::Builder as RtBuilder;
use zbus::{names::OwnedWellKnownName, zvariant::OwnedObjectPath};

mod client;
mod format;
mod server;
mod timeout;

type Result<T = (), E = Error> = std::result::Result<T, E>;

#[cfg(debug_assertions)]
const SERVER_NAME_STR: &str = "club.bnuy.debug.Empress";

#[cfg(not(debug_assertions))]
const SERVER_NAME_STR: &str = "club.bnuy.Empress";

#[cfg(test)]
static INTERFACE_ID: LazyLock<String> =
    LazyLock::new(|| SERVER_NAME_STR.replace(".debug", "") + ".Daemon");

static SERVER_NAME: LazyLock<OwnedWellKnownName> =
    LazyLock::new(|| SERVER_NAME_STR.try_into().unwrap());
static SERVER_PATH: LazyLock<OwnedObjectPath> =
    LazyLock::new(|| "/club/bnuy/Empress/Daemon".try_into().unwrap());

#[cfg(test)]
#[test]
fn assert_path() {
    assert_eq!(
        SERVER_PATH
            .as_str()
            .strip_prefix('/')
            .unwrap()
            .replace('/', "."),
        *INTERFACE_ID
    );
}

mod opts {
    use std::{io::IsTerminal, sync::LazyLock};

    use anyhow::{anyhow, Context};
    use log::LevelFilter;

    use crate::{
        server::{self, mpris::player},
        Result,
    };

    include!("opts.rs");

    impl Opts {
        pub fn into_inner(self) -> Command {
            let Self { log, cmd } = self;
            log.init().unwrap();
            cmd
        }
    }

    impl LogOpts {
        pub fn init(self) -> Result {
            const VERBOSITY: [LevelFilter; 3] =
                [LevelFilter::Info, LevelFilter::Debug, LevelFilter::Trace];
            #[cfg(debug_assertions)]
            const DEFAULT_V: u8 = 1;
            #[cfg(not(debug_assertions))]
            const DEFAULT_V: u8 = 0;

            let Self {
                quiet,
                no_quiet,
                verbose,
            } = self;

            env_logger::builder()
                .parse_default_env()
                .filter_level(
                    if quiet || !(no_quiet || verbose != 0 || std::io::stderr().is_terminal()) {
                        LevelFilter::Warn
                    } else {
                        VERBOSITY[usize::from(DEFAULT_V.saturating_add(verbose))
                            .min(VERBOSITY.len().saturating_sub(1))]
                    },
                )
                .try_init()
                .map_err(Into::into)
        }
    }

    impl From<PlaybackStatus> for player::PlaybackStatus {
        fn from(value: PlaybackStatus) -> Self {
            match value {
                PlaybackStatus::Playing => player::PlaybackStatus::Playing,
                PlaybackStatus::Paused => player::PlaybackStatus::Paused,
                PlaybackStatus::Stopped => player::PlaybackStatus::Stopped,
            }
        }
    }

    impl From<PlayerOpts> for server::PlayerOpts {
        fn from(value: PlayerOpts) -> Self {
            let PlayerOpts { bus, ibus, state } = value;

            let (bus, ignore_bus_case) = match (bus, ibus) {
                (None, None) => (String::new(), false),
                (Some(i), None) => (i, false),
                (None, Some(i)) => (i, true),
                (Some(_), Some(_)) => unreachable!(),
            };

            server::PlayerOpts {
                bus,
                ignore_bus_case,
                states: state.into_iter().map(Into::into).collect(),
            }
        }
    }

    impl std::str::FromStr for Offset {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            use regex::Regex;

            static SEEK_PATTERN: LazyLock<Regex> =
                LazyLock::new(|| Regex::new(r"(\d*(?:\.\d+)?)(\+|-)?").unwrap());

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
}

fn main() {
    let result = run();

    match result {
        Ok(()) => (),
        Err(e) => {
            opts::LogOpts::default().init().ok();

            error!("{e:?}");
        },
    }
}

fn run() -> Result {
    let rt = RtBuilder::new_current_thread()
        .enable_all()
        .build()
        .context("Error starting async runtime")?;

    match opts::Opts::parse().into_inner() {
        opts::Command::Server => rt.block_on(server::run()),
        opts::Command::Client(id) => rt.block_on(client::run(id)),
    }
}
