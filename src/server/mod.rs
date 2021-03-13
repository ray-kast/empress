use std::{future::Future, marker::PhantomData, sync::Arc};

use anyhow::{Context, Error};
use dbus::{
    arg::{AppendAll, ArgAll},
    channel::MatchingReceiver,
    message::MatchRule,
    MethodErr,
};
use dbus_crossroads::{Context as CrContext, Crossroads, IfaceBuilder};
use dbus_tokio::connection;
use log::{error, info, warn};
use tokio::{
    select,
    signal::{unix, unix::SignalKind},
    sync::oneshot,
};

use crate::{MethodId, PlayerOpts, Result, INTERFACE_NAME, SERVER_NAME, SERVER_PATH};

mod mpris;
mod player;
mod player_map;
#[allow(clippy::module_inception)] // I'm aware, but the struct is called Server
mod server;

pub(self) use player::Player;
pub(self) use player_map::PlayerMap;

pub(self) type MethodResult<T = ()> = Result<T, MethodErr>;

pub(self) fn method_err(
    method: impl std::fmt::Display,
    e: impl Into<Error>,
    msg: impl std::fmt::Display,
) -> MethodErr {
    error!(
        "Method hander for {} failed: {:?}",
        method,
        e.into().context(msg.to_string())
    );
    MethodErr::failed(&msg)
}

use server::Server;

fn handle<
    OA: ArgAll + AppendAll,
    F: FnMut(Arc<Server>) -> R + Send + 'static,
    R: Future<Output = MethodResult<OA>>,
>(
    mut ctx: CrContext,
    cr: &mut Crossroads,
    mut f: F,
) -> impl Future<Output = PhantomData<OA>> {
    let serv = cr.data_mut::<Arc<Server>>(ctx.path()).cloned();

    async move {
        let serv = match serv {
            Some(s) => s,
            None => return ctx.reply(Err(MethodErr::no_path(ctx.path()))),
        };

        ctx.reply(f(serv).await)
    }
}

fn register_interface(b: &mut IfaceBuilder<Arc<Server>>) {
    b.method_with_cr_async(
        MethodId::ListPlayers.to_string(),
        (),
        ("players",),
        |ctx, cr, ()| handle(ctx, cr, |serv| async move { serv.list_players().await }),
    );

    b.method_with_cr_async(MethodId::Next.to_string(), (), (), |ctx, cr, ()| {
        handle(
            ctx,
            cr,
            |serv| async move { serv.next(PlayerOpts {}).await },
        )
    });

    b.method_with_cr_async(MethodId::Previous.to_string(), (), (), |ctx, cr, ()| {
        handle(
            ctx,
            cr,
            |serv| async move { serv.prev(PlayerOpts {}).await },
        )
    });

    b.method_with_cr_async(MethodId::Pause.to_string(), (), (), |ctx, cr, ()| {
        handle(
            ctx,
            cr,
            |serv| async move { serv.pause(PlayerOpts {}).await },
        )
    });

    b.method_with_cr_async(MethodId::PlayPause.to_string(), (), (), |ctx, cr, ()| {
        handle(ctx, cr, |serv| async move {
            serv.play_pause(PlayerOpts {}).await
        })
    });

    b.method_with_cr_async(MethodId::Stop.to_string(), (), (), |ctx, cr, ()| {
        handle(
            ctx,
            cr,
            |serv| async move { serv.stop(PlayerOpts {}).await },
        )
    });

    b.method_with_cr_async(MethodId::Play.to_string(), (), (), |ctx, cr, ()| {
        handle(
            ctx,
            cr,
            |serv| async move { serv.play(PlayerOpts {}).await },
        )
    });

    b.method_with_cr_async(
        MethodId::SeekRelative.to_string(),
        ("to",),
        ("secs",),
        |ctx, cr, (to,)| {
            handle(ctx, cr, move |serv| async move {
                serv.seek_relative(PlayerOpts {}, to).await
            })
        },
    );

    b.method_with_cr_async(
        MethodId::SeekAbsolute.to_string(),
        ("to",),
        ("secs",),
        |ctx, cr, (to,)| {
            handle(ctx, cr, move |serv| async move {
                serv.seek_absolute(PlayerOpts {}, to).await
            })
        },
    );
}

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

    let tok = cr.register(&*INTERFACE_NAME, register_interface);

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
                Err(()) => warn!("Failed to handle message {}", msg_dbg),
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
        Some(()) = int.recv() => {
            if atty::is(atty::Stream::Stdin) && atty::is(atty::Stream::Stderr) {
                eprintln!();
            }

            Ok(())
        }
        Some(()) = quit.recv() => Ok(()),
        Some(()) = term.recv() => Ok(()),
        res = close_rx => Err(
            res.context("lost D-Bus connection resource").map_or_else(|e| e, |e| e)
        ),
    )?;

    info!("Shutting down...");

    Ok(())
}
