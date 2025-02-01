use std::{
    collections::{HashMap, HashSet},
    fmt,
    io::IsTerminal,
};

use anyhow::{Context, Error};
use log::{error, info};
use regex::{Regex, RegexBuilder};
use server::Server;
use tokio::{
    select,
    signal::{unix, unix::SignalKind},
};
use zbus::{
    connection,
    fdo::{Error as ZError, Result as ZResult},
    zvariant::{self, OwnedValue},
};

use self::mpris::player::PlaybackStatus;
use crate::{Result, SERVER_NAME, SERVER_PATH};

pub mod mpris;
mod player;
mod player_map;
mod position;
mod signal_matcher;
// TODO: rename Server to something like Handler
#[expect(
    clippy::module_inception,
    reason = "This is the module for the Server struct"
)]
mod server;

pub use position::Position;

#[derive(
    Debug,
    Clone,
    Default,
    PartialEq,
    zvariant::SerializeDict,
    zvariant::DeserializeDict,
    zvariant::Type,
)]
#[zvariant(signature = "dict")]
pub struct PlayerOpts {
    pub bus: String,
    pub ignore_bus_case: bool,
    pub states: Vec<PlaybackStatus>,
}

impl PlayerOpts {
    pub fn build(&self) -> Result<PlayerMatcher> {
        let Self {
            bus,
            ignore_bus_case,
            states,
        } = self;

        Ok(PlayerMatcher {
            bus: RegexBuilder::new(bus)
                .case_insensitive(*ignore_bus_case)
                .build()
                .context("Invalid regular expression")?,
            states: states.iter().copied().collect(),
        })
    }
}

pub trait MatchPlayer {
    fn bus(&self) -> &str;
    fn status(&self) -> PlaybackStatus;
}

pub struct PlayerMatcher {
    bus: Regex,
    states: HashSet<PlaybackStatus>,
}

impl PlayerMatcher {
    pub fn is_match(&self, p: &impl MatchPlayer) -> bool {
        self.bus.is_match(p.bus()) && (self.states.is_empty() || self.states.contains(&p.status()))
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    Default,
    serde::Serialize,
    serde::Deserialize,
    zvariant::Type,
    zvariant::Value,
    zvariant::OwnedValue,
)]
pub enum PlayerStatusKind {
    #[default]
    NoPlayer = 0,
    NoPosition = 1,
    Default = 2,
}

// !Clone because of OwnedValue
#[derive(
    Debug,
    Default,
    serde::Serialize,
    serde::Deserialize,
    zvariant::Type,
    zvariant::Value,
    zvariant::OwnedValue,
)]
pub struct PlayerStatus {
    pub kind: PlayerStatusKind,
    pub bus: String,
    pub ident: String,
    pub status: PlaybackStatus,
    pub volume: f64,
    pub rate: f64,
    pub position: i64,
    pub metadata: HashMap<String, OwnedValue>,
}

pub type PlayerList = Vec<(String, PlaybackStatus)>;

use player::Player;
use player_map::PlayerMap;

fn method_err(e: impl Into<Error>, msg: impl fmt::Display) -> ZError {
    let msg = msg.to_string();
    error!(
        "Method handler returned an error: {:?}",
        e.into().context(msg.clone())
    );
    ZError::Failed(msg)
}

pub async fn run() -> Result {
    let conn = connection::Builder::session()
        .context("Error creating connection builder")?
        .build()
        .await
        .context("Error connecting to D-Bus")?;

    let (srv, join) = Server::new(conn.clone(), SERVER_PATH.as_ref()).await?;
    let created = conn
        .object_server()
        .at(&*SERVER_PATH, srv)
        .await
        .context("Error registering server")?;

    if !created {
        anyhow::bail!("Object server already exists");
    }

    conn.request_name(&*SERVER_NAME)
        .await
        .context("Error requesting server name")?;

    let mut hup = unix::signal(SignalKind::hangup()).context("Error hooking SIGHUP")?;
    let mut int = unix::signal(SignalKind::interrupt()).context("Error hooking SIGINT")?;
    let mut quit = unix::signal(SignalKind::quit()).context("Error hooking SIGQUIT")?;
    let mut term = unix::signal(SignalKind::terminate()).context("Error hooking SIGTERM")?;

    select!(
        Some(()) = hup.recv() => (),
        Some(()) = int.recv() => {
            if std::io::stdin().is_terminal() && std::io::stderr().is_terminal() {
                eprintln!();
            }
        }
        Some(()) = quit.recv() => (),
        Some(()) = term.recv() => (),
    );

    info!("Shutting down...");

    let srv = conn
        .object_server()
        .remove::<Server, _>(&*SERVER_PATH)
        .await
        .context("Error unregistering server")?;

    if !srv {
        unreachable!("Object server not found at expected registered path");
    }

    join.await.context("Error shutting down server")?;

    Ok(())
}
