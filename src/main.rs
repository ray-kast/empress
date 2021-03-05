use anyhow::{Context, Error};
use lazy_static::lazy_static;
use structopt::StructOpt;
use tokio::runtime::Builder as RtBuilder;

mod client;
mod server;

type Result<T = (), E = Error> = std::result::Result<T, E>;

lazy_static! {
    static ref API_IDENT: String = format!("Empress{}", env!("CARGO_PKG_VERSION_MAJOR"));
    static ref NAME_PREFIX: String = format!("net.ryan_s.{}", *API_IDENT);
    static ref PATH_PREFIX: String = format!("/net/ryan_s/{}", *API_IDENT);
    static ref INTERFACE_NAME: String = format!("{}.Daemon", *NAME_PREFIX);
    static ref SERVER_NAME: String = NAME_PREFIX.clone();
    static ref SERVER_PATH: String = format!("{}/Daemon", *PATH_PREFIX);
}

#[derive(Debug, Clone, Copy, StructOpt)]
enum MethodId {
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

const METHOD_IDS: &[MethodId] = &[
    MethodId::Next,
    MethodId::Previous,
    MethodId::Pause,
    MethodId::PlayPause,
    MethodId::Stop,
    MethodId::Play,
];

impl std::fmt::Display for MethodId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
    Client(MethodId),
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
