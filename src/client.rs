use std::{sync::Arc, time::Duration};

use anyhow::{Context, Error};
use dbus::{
    arg::ReadAll,
    nonblock::{Proxy, SyncConnection},
    strings::Member,
};
use dbus_tokio::connection;
use tokio::{select, sync::oneshot, task};

use crate::{ClientCommand, ExtraMethodId, Result, INTERFACE_NAME, SERVER_NAME, SERVER_PATH};

pub(super) async fn run(cmd: ClientCommand) -> Result {
    let (res, conn) = connection::new_session_sync().context("failed to connect to D-Bus")?;
    let (close_tx, close_rx) = oneshot::channel();

    task::spawn(async {
        close_tx
            .send(Error::from(res.await).context("D-Bus disconnected"))
            .ok();
    });

    let run = async move {
        let proxy = Proxy::new(&*SERVER_NAME, &*SERVER_PATH, Duration::from_secs(2), conn);

        match cmd {
            ClientCommand::Extra(e) => match e {
                ExtraMethodId::ListPlayers => {
                    let (players,): (Vec<(String, String)>,) =
                        try_send(&proxy, e.to_string()).await?;

                    for (player, status) in players {
                        println!("{}\t{}", player, status);
                    }
                },
            },
            ClientCommand::Control(c) => {
                try_send(&proxy, c.to_string()).await?;
            },
        }

        Ok(())
    };

    select!(
        res = run => res,
        err = close_rx => Err(
            err.context("lost D-Bus connection resource").map_or_else(|e| e, |e| e)
        ),
    )
}

async fn try_send<T: ReadAll + 'static, M: for<'a> Into<Member<'a>>>(
    proxy: &Proxy<'_, Arc<SyncConnection>>,
    method: M,
) -> Result<T> {
    const MAX_TRIES: usize = 5;

    let method = method.into();
    let mut i = 0;

    loop {
        match proxy
            .method_call(&*INTERFACE_NAME, &method, ())
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
