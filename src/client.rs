use std::{sync::Arc, time::Duration};

use anyhow::{Context, Error};
use dbus::nonblock::{Proxy, SyncConnection};
use dbus_tokio::connection;
use tokio::{select, sync::oneshot, task};

use crate::{MethodId, Result, INTERFACE_NAME, SERVER_NAME, SERVER_PATH};

pub(super) async fn run(id: MethodId) -> Result {
    let (res, conn) = connection::new_session_sync().context("failed to connect to D-Bus")?;
    let (close_tx, close_rx) = oneshot::channel();

    task::spawn(async {
        close_tx
            .send(Error::from(res.await).context("D-Bus disconnected"))
            .ok();
    });

    let run = async move {
        let proxy = Proxy::new(&*SERVER_NAME, &*SERVER_PATH, Duration::from_secs(2), conn);

        try_send(&proxy, id).await?;

        Ok(())
    };

    select!(
        res = run => res,
        err = close_rx => Err(
            err.context("lost D-Bus connection resource").map_or_else(|e| e, |e| e)
        ),
    )
}

async fn try_send(proxy: &Proxy<'_, Arc<SyncConnection>>, id: MethodId) -> Result {
    const MAX_TRIES: usize = 5;

    let mut i = 0;

    loop {
        match proxy
            .method_call(&*INTERFACE_NAME, id.to_string(), ())
            .await
            .context("failed to contact empress server")
        {
            Err(e) if i < MAX_TRIES => eprintln!("WARNING: {:?}", e),
            r => break r,
        }

        i += 1;
        eprintln!("Retry attempt {:?}", i);

        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}
