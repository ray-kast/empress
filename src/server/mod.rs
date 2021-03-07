use std::sync::Arc;

use anyhow::{Context, Error};
use dbus::{channel::MatchingReceiver, message::MatchRule, MethodErr};
use dbus_crossroads::{Crossroads, IfaceBuilder};
use dbus_tokio::connection;
use tokio::{
    select,
    signal::{unix, unix::SignalKind},
    sync::oneshot,
};

use crate::{ExtraMethodId, Result, CONTROL_METHOD_IDS, INTERFACE_NAME, SERVER_NAME, SERVER_PATH};

mod mpris;
mod player;
mod player_map;
#[allow(clippy::module_inception)] // I'm aware, but the struct is called Server
mod server;

pub(self) use player::Player;
pub(self) use player_map::PlayerMap;

pub(self) fn method_err(e: impl Into<Error>, msg: impl std::fmt::Display) -> MethodErr {
    eprintln!("ERROR: {:?}", e.into().context(msg.to_string()));
    MethodErr::failed(&msg)
}

use server::Server;

pub async fn run() -> Result {
    let (res, conn) = connection::new_session_sync().context("failed to connect to D-Bus")?;
    let (close_tx, close_rx) = oneshot::channel();

    tokio::spawn(async {
        close_tx
            .send(Error::from(res.await).context("D-Bus disconnected"))
            .ok();
    });

    conn.request_name(&*SERVER_NAME, false, true, false)
        .await
        .context("failed to request server name")?;

    let mut cr = Crossroads::new();

    cr.set_async_support(Some((
        conn.clone(),
        Box::new(|x| {
            tokio::spawn(x);
        }),
    )));

    let tok = cr.register(&*INTERFACE_NAME, |b: &mut IfaceBuilder<Arc<Server>>| {
        b.method_with_cr_async(
            ExtraMethodId::ListPlayers.to_string(),
            (),
            ("players",),
            move |mut ctx, cr, ()| {
                let serv = cr.data_mut::<Arc<Server>>(ctx.path()).cloned();

                async move {
                    let serv = match serv {
                        Some(s) => s,
                        None => return ctx.reply(Err(MethodErr::no_path(ctx.path()))),
                    };

                    let res = serv.handle_list_players().await;

                    ctx.reply(res)
                }
            },
        );

        for id in CONTROL_METHOD_IDS.iter().copied() {
            b.method_with_cr_async(id.to_string(), (), (), move |mut ctx, cr, ()| {
                let serv = cr.data_mut::<Arc<Server>>(ctx.path()).cloned();

                async move {
                    let serv = match serv {
                        Some(s) => s,
                        None => return ctx.reply(Err(MethodErr::no_path(ctx.path()))),
                    };

                    let res = serv.handle_control(id).await;

                    ctx.reply(res)
                }
            });
        }
    });

    cr.insert(
        &*SERVER_PATH,
        &[tok],
        Server::new(conn.clone())
            .await
            .context("failed to initialize server")?,
    );

    conn.start_receive(
        MatchRule::new_method_call(),
        Box::new(move |msg, conn| {
            let msg_dbg = format!("{:?}", msg);

            match cr.handle_message(msg, conn) {
                Ok(()) => (),
                Err(()) => eprintln!("WARNING: failed to handle message {}", msg_dbg),
            };
            true
        }),
    );

    let mut hup = unix::signal(SignalKind::hangup()).context("failed to hook SIGHUP")?;
    let mut int = unix::signal(SignalKind::interrupt()).context("failed to hook SIGINT")?;
    let mut quit = unix::signal(SignalKind::quit()).context("failed to hook SIGQUIT")?;
    let mut term = unix::signal(SignalKind::terminate()).context("failed to hook SIGTERM")?;

    select!(
        Some(()) = hup.recv() => Ok(()),
        Some(()) = int.recv() => Ok(()),
        Some(()) = quit.recv() => Ok(()),
        Some(()) = term.recv() => Ok(()),
        res = close_rx => Err(
            res.context("lost D-Bus connection resource").map_or_else(|e| e, |e| e)
        ),
    )?;

    eprintln!("Shutting down...");

    Ok(())
}
