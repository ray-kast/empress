use std::{collections::HashMap, fmt};

use anyhow::{Context, Error};
use log::{error, info};
use server::Server;
use tokio::{
    select,
    signal::{unix, unix::SignalKind},
};
use zbus::{
    fdo::{Error as ZError, Result as ZResult},
    zvariant::{self, OwnedValue},
    ConnectionBuilder,
};

use self::mpris::player::PlaybackStatus;
use crate::{Result, SERVER_NAME, SERVER_PATH};

pub mod mpris;
mod player;
mod player_map;
// TODO: deal with this
#[allow(clippy::module_inception)] // I'm aware, but the struct is called Server
mod server;

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize, zvariant::Type)]
pub enum PlayerStatusKind {
    #[default]
    NoPlayer,
    NoPosition,
    Default,
}

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize, zvariant::Type)]
pub struct PlayerStatus {
    pub kind: PlayerStatusKind,
    pub bus: String,
    pub ident: String,
    pub status: PlaybackStatus,
    pub position: i64,
    pub metadata: HashMap<String, OwnedValue>,
}

pub type PlayerList = Vec<(String, PlaybackStatus)>;

pub(self) use player::Player;
pub(self) use player_map::PlayerMap;

pub(self) fn method_err(e: impl Into<Error>, msg: impl fmt::Display) -> ZError {
    let msg = msg.to_string();
    error!("Method hander failed: {:?}", e.into().context(msg.clone()));
    ZError::Failed(msg)
}

pub async fn run() -> Result {
    let conn = ConnectionBuilder::session()
        .context("Failed to create connection builder")?
        .build()
        .await
        .context("Failed to connect to D-Bus")?;

    let (srv, join) = Server::new(conn.clone()).await?;
    let created = conn
        .object_server()
        .at(&*SERVER_PATH, srv)
        .await
        .context("Failed to register server")?;

    if !created {
        anyhow::bail!("Object server already exists");
    }

    conn.request_name(&*SERVER_NAME)
        .await
        .context("Failed to request server name")?;

    let mut hup = unix::signal(SignalKind::hangup()).context("Failed to hook SIGHUP")?;
    let mut int = unix::signal(SignalKind::interrupt()).context("Failed to hook SIGINT")?;
    let mut quit = unix::signal(SignalKind::quit()).context("Failed to hook SIGQUIT")?;
    let mut term = unix::signal(SignalKind::terminate()).context("Failed to hook SIGTERM")?;

    select!(
        Some(()) = hup.recv() => (),
        Some(()) = int.recv() => {
            if atty::is(atty::Stream::Stdin) && atty::is(atty::Stream::Stderr) {
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
