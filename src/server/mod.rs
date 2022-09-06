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
    zvariant::{OwnedValue, Value},
    ConnectionBuilder,
};

use crate::{Result, SERVER_NAME, SERVER_PATH};

pub mod mpris;
mod player;
mod player_map;
#[allow(clippy::module_inception)] // I'm aware, but the struct is called Server
mod server;

pub type NowPlayingResponse<'a> = (HashMap<String, Value<'a>>, String);
pub type OwnedNowPlayingResponse = (HashMap<String, OwnedValue>, String);

pub(self) use player::Player;
pub(self) use player_map::PlayerMap;

pub(self) fn method_err(
    method: impl fmt::Display,
    e: impl Into<Error>,
    msg: impl fmt::Display,
) -> ZError {
    let msg = msg.to_string();
    error!(
        "Method hander for {} failed: {:?}",
        method,
        e.into().context(msg.clone())
    );
    ZError::Failed(msg)
}

pub async fn run() -> Result {
    let conn = ConnectionBuilder::session()
        .context("failed to create connection builder")?
        .build()
        .await
        .context("failed to connect to D-Bus")?;

    conn.object_server()
        .at(&*SERVER_PATH, Server::new(conn.clone()).await?)
        .await
        .context("failed to register server")?;

    conn.request_name(&*SERVER_NAME)
        .await
        .context("failed to request server name")?;

    let mut hup = unix::signal(SignalKind::hangup()).context("failed to hook SIGHUP")?;
    let mut int = unix::signal(SignalKind::interrupt()).context("failed to hook SIGINT")?;
    let mut quit = unix::signal(SignalKind::quit()).context("failed to hook SIGQUIT")?;
    let mut term = unix::signal(SignalKind::terminate()).context("failed to hook SIGTERM")?;

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

    Ok(())
}
