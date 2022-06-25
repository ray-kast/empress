use anyhow::Context;

use crate::{
    interface::{Daemon, SERVER_NAME, SERVER_PATH},
    Result,
};

pub mod handler;
pub(self) mod player;
pub(self) mod player_map;

pub use handler::Handler;

async fn connect() -> zbus::Result<()> {
    let handler = handler::Handler::new();

    let conn = zbus::ConnectionBuilder::session()?
        .name(&*SERVER_NAME)?
        .serve_at(&*SERVER_PATH, Daemon(handler))?
        .build()
        .await?;

    Ok(())
}

pub async fn run() -> Result {
    let conn = connect().await.context("Failed to connect to D-Bus")?;

    std::future::pending::<()>().await;
    std::mem::drop(conn);
    Ok(())
}
