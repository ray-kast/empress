use std::time::Duration;

use anyhow::{Context, Error};
use dbus::nonblock::Proxy;
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

        let () = proxy
            .method_call(&*INTERFACE_NAME, id.to_string(), ())
            .await
            .context("failed to contact empress server")?;

        Ok(())
    };

    select!(
        res = run => res,
        err = close_rx => Err(
            err.context("lost D-Bus connection resource").map_or_else(|e| e, |e| e)
        ),
    )
}
