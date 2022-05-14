use anyhow::Context;

use crate::{
    interface::{Daemon, SERVER_NAME, SERVER_PATH},
    Result,
};

async fn connect() -> zbus::Result<zbus::Connection> {
    zbus::ConnectionBuilder::session()?
        .name(&*SERVER_NAME)?
        .serve_at(&*SERVER_PATH, Daemon)?
        .build()
        .await
}

pub async fn run() -> Result {
    let conn = connect().await.context("Failed to connect to D-Bus")?;

    std::future::pending::<()>().await;
    std::mem::drop(conn);
    Ok(())
}
