#![warn(missing_docs, clippy::all, clippy::pedantic, clippy::cargo)]
#![deny(broken_intra_doc_links, missing_debug_implementations)]

//! Binary crate for `empress`.  See [the
//! README](https://github.com/ray-kast/empress/blob/master/README.md) for more
//! details.

use std::{
    fmt,
    fmt::{Display, Formatter},
};

use anyhow::{Context, Error};
use lazy_static::lazy_static;
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

#[derive(Debug, Clone, Copy, StructOpt)]
enum ExtraMethodId {
    /// List the players currently tracked by the daemon
    ListPlayers,
}

#[derive(Debug, Clone, Copy, StructOpt)]
enum ControlMethodId {
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
}

const CONTROL_METHOD_IDS: &[ControlMethodId] = &[
    ControlMethodId::Next,
    ControlMethodId::Previous,
    ControlMethodId::Pause,
    ControlMethodId::PlayPause,
    ControlMethodId::Stop,
    ControlMethodId::Play,
];

impl Display for ExtraMethodId {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::ListPlayers => "ListPlayers",
        })
    }
}

impl Display for ControlMethodId {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Next => "Next",
            Self::Previous => "Previous",
            Self::Pause => "Pause",
            Self::PlayPause => "PlayPause",
            Self::Stop => "Stop",
            Self::Play => "Play",
        })
    }
}

#[derive(StructOpt)]
enum Opts {
    /// Launch a D-Bus service abstracting MPRIS players
    Server,
    #[structopt(flatten)]
    Client(ClientCommand),
}

#[derive(StructOpt)]
enum ClientCommand {
    #[structopt(flatten)]
    Extra(ExtraMethodId),
    #[structopt(flatten)]
    Control(ControlMethodId),
}

fn main() {
    let result = run();

    match result {
        Ok(()) => (),
        Err(e) => eprintln!("ERROR: {:?}", e),
    }
}

fn run() -> Result {
    let rt = RtBuilder::new_current_thread()
        .enable_all()
        .build()
        .context("failed to start runtime")?;

    let opts = Opts::from_args();

    match opts {
        Opts::Server => rt.block_on(server::run()),
        Opts::Client(id) => rt.block_on(client::run(id)),
    }
}
