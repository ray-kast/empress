use std::{
    future::Future,
    io::{self, Write},
    time::Duration,
};

use anyhow::Context;
use log::{info, warn};
use zbus::{connection, fdo, Connection};

use self::proxy::EmpressProxy;
use crate::{
    server::{self, mpris::player},
    timeout::{self, Timeout},
    ClientCommand, Offset, Result, SERVER_NAME,
};

mod now_playing;
mod proxy;

// TODO: AsyncFn wen eta son and also async lifetimes are So Miserable
trait ProxyCommand<'p> {
    type Output;
    type Then;

    fn run(&self, proxy: &EmpressProxy<'p>) -> impl Future<Output = fdo::Result<Self::Output>>;

    fn then(&self, out: Self::Output) -> Result<Self::Then>;
}

macro_rules! cmd {
    (
        $cmd:ident$(($($arg_ty:ty),+$(,)?))? -> $out_ty:ty;
        |$p:ident$(, $($arg:pat_param),+)?$(,)?| $expr:expr
        $(, |$io:tt, $out:pat_param| $out_expr:expr)?
        $(,)?
    ) => {
        struct $cmd$(($($arg_ty),*))?;
        impl<'p> ProxyCommand<'p> for $cmd {
            type Output = $out_ty;

            async fn run(&self, $p: &EmpressProxy<'p>) -> fdo::Result<$out_ty> {
                let Self$(($($arg,)+))? = self;
                $expr
            }

            cmd!(@then $($io, $out, $out_expr)?);
        }
    };

    (@then) => {
        type Then = Self::Output;

        #[inline]
        fn then(&self, out: Self::Output) -> Result<Self::Output> { Ok(out) }
    };

    (@then _, $out:pat_param, $expr:expr) => {
        type Then = ();

        fn then(&self, $out: Self::Output) -> Result<()> {
            $expr;
            Ok(())
        }
    };

    (@then $io:ident, $out:pat_param, $expr:expr) => {
        type Then = ();

        fn then(&self, $out: Self::Output) -> Result<()> {
            let mut $io = io::stdout().lock();
            $expr;
            $io.flush().map_err(Into::into)
        }
    };
}

cmd! {
    Scan -> Vec<String>;
    |p| p.scan().await,
    |_, l| if l.is_empty() {
        info!("No changes detected");
    } else {
        for line in l {
            warn!("Change detected: {line}");
        }
    },
}
cmd! {
    ListPlayers -> Vec<(String, player::PlaybackStatus)>;
    |p| p.list_players().await,
    |i, l| for (player, status) in l {
        writeln!(i, "{player}\t{status}")?;
    },
}
cmd! {
    Raise(server::PlayerOpts) -> ();
    |p, o| p.raise(o).await
}
cmd! {
    Next(server::PlayerOpts) -> ();
    |p, o| p.next(o).await
}
cmd! {
    Previous(server::PlayerOpts) -> ();
    |p, o| p.prev(o).await
}
cmd! {
    Pause(server::PlayerOpts) -> ();
    |p, o| p.pause(o).await
}
cmd! {
    PlayPause(server::PlayerOpts) -> ();
    |p, o| p.play_pause(o).await
}
cmd! {
    Stop(server::PlayerOpts) -> ();
    |p, o| p.stop(o).await
}
cmd! {
    Play(server::PlayerOpts) -> ();
    |p, o| p.play(o).await
}
cmd! {
    Seek(server::PlayerOpts, Offset) -> f64;
    |p, o, t| match *t {
        Offset::Relative(t) => p.seek_relative(o, t).await,
        Offset::Absolute(t) => p.seek_absolute(o, t).await,
    },
    |i, t| writeln!(i, "{t}")?,
}
cmd! {
    Volume(server::PlayerOpts, Offset) -> f64;
    |p, o, v| match *v {
        Offset::Relative(v) => p.vol_relative(o, v).await,
        Offset::Absolute(v) => p.vol_absolute(o, v).await,
    },
    |i, v| writeln!(i, "{v}")?,
}
cmd! {
    SwitchCurrent(String, bool) -> ();
    |p, t, w| p.switch_current(t, *w).await
}

pub(super) async fn run(cmd: ClientCommand) -> Result {
    let conn = connection::Builder::session()
        .context("Error creatihng session connection builder")?
        .build()
        .await
        .context("Error connecting to D-Bus")?;

    match cmd {
        ClientCommand::Scan => try_run(&conn, Scan).await,
        ClientCommand::ListPlayers => try_run(&conn, ListPlayers).await,
        ClientCommand::NowPlaying(o) => now_playing::run(&conn, o).await,
        ClientCommand::Raise(o) => try_run(&conn, Raise(o.into())).await,
        ClientCommand::Next(o) => try_run(&conn, Next(o.into())).await,
        ClientCommand::Previous(o) => try_run(&conn, Previous(o.into())).await,
        ClientCommand::Pause(o) => try_run(&conn, Pause(o.into())).await,
        ClientCommand::PlayPause(o) => try_run(&conn, PlayPause(o.into())).await,
        ClientCommand::Stop(o) => try_run(&conn, Stop(o.into())).await,
        ClientCommand::Play(o) => try_run(&conn, Play(o.into())).await,
        ClientCommand::Seek { player, to } => try_run(&conn, Seek(player.into(), to)).await,
        ClientCommand::Volume { player, vol } => try_run(&conn, Volume(player.into(), vol)).await,
        ClientCommand::SwitchCurrent { to, no_play } => {
            try_run(&conn, SwitchCurrent(to, !no_play)).await
        },
    }
}

async fn try_run<'p, C: ProxyCommand<'p>>(conn: &'p Connection, cmd: C) -> Result<C::Then> {
    const TRIES: u32 = 5;
    const DELAY_MILLIS: u64 = 5;

    let mut i = 0;

    let res = 'dial: loop {
        let proxy = dial(conn).await?;
        let mut redial = false;

        while !redial {
            match proxy.try_run(Duration::from_secs(2), |p| cmd.run(p)).await {
                Err(e) if i < TRIES => {
                    warn!("Request failed: {e:?}");

                    if matches!(e, timeout::Error::Other(fdo::Error::ServiceUnknown(_))) {
                        redial = true;
                    }
                },
                r => break 'dial r.context("Unable to contact empress server")?,
            }

            i += 1;
            info!("Retry attempt {i}");

            tokio::time::sleep(Duration::from_millis(DELAY_MILLIS << i)).await;
            assert!(i <= TRIES);
        }
    };

    cmd.then(res)
}

async fn dial(conn: &Connection) -> Result<Timeout<EmpressProxy<'_>>> {
    Ok(Timeout::from(
        proxy::EmpressProxy::builder(conn)
            .destination(&*SERVER_NAME)
            .context("Error setting empress proxy destination")?
            .build()
            .await
            .context("Error building server proxy")?,
    ))
}
