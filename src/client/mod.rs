use std::{future::Future, io::IsTerminal, time::Duration};

use anyhow::Context;
use log::{info, warn};
use zbus::connection;

use self::proxy::EmpressProxy;
use crate::{timeout::Timeout, ClientCommand, Offset, Result, SERVER_NAME};

mod now_playing;
mod proxy;

macro_rules! courtesy_line {
    () => {
        if std::io::stdout().is_terminal() {
            println!();
        }
    };
}

#[expect(
    clippy::too_many_lines,
    reason = "Somewhat unavoidably long match block"
)]
pub(super) async fn run(cmd: ClientCommand) -> Result {
    let conn = connection::Builder::session()
        .context("Error creatihng session connection builder")?
        .build()
        .await
        .context("Error connecting to D-Bus")?;

    let proxy = Timeout::from(
        proxy::EmpressProxy::builder(&conn)
            .destination(&*SERVER_NAME)
            .context("Error setting empress proxy destination")?
            .build()
            .await
            .context("Error building server proxy")?,
    );

    match cmd {
        ClientCommand::Scan => {
            let log = try_send(&proxy, EmpressProxy::scan).await?;

            if log.is_empty() {
                info!("No changes detected");
            } else {
                for line in log {
                    warn!("Change detected: {line}");
                }
            }
        },
        ClientCommand::Raise(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.raise(&opts)).await?;
        },
        ClientCommand::Next(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.next(&opts)).await?;
        },
        ClientCommand::Previous(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.prev(&opts)).await?;
        },
        ClientCommand::Pause(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.pause(&opts)).await?;
        },
        ClientCommand::PlayPause(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.play_pause(&opts)).await?;
        },
        ClientCommand::Stop(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.stop(&opts)).await?;
        },
        ClientCommand::Play(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.play(&opts)).await?;
        },
        ClientCommand::ListPlayers => {
            let players = try_send(&proxy, EmpressProxy::list_players).await?;

            for (player, status) in players {
                println!("{player}\t{status}");
            }
        },
        ClientCommand::NowPlaying {
            player,
            format,
            watch,
            zero,
        } => now_playing::run(proxy, player, format, watch, zero).await?,
        ClientCommand::Seek {
            player,
            to: Offset::Relative(to),
        } => {
            let player = player.into();
            try_send(&proxy, |p| p.seek_relative(&player, to)).await?;
        },
        ClientCommand::Seek {
            player,
            to: Offset::Absolute(to),
        } => {
            let player = player.into();
            try_send(&proxy, |p| p.seek_absolute(&player, to)).await?;
        },
        ClientCommand::Volume {
            player,
            vol: Offset::Relative(to),
        } => {
            let player = player.into();
            let vol = try_send(&proxy, |p| p.vol_relative(&player, to)).await?;

            print!("{vol}");
            courtesy_line!();
        },
        ClientCommand::Volume {
            player,
            vol: Offset::Absolute(to),
        } => {
            let player = player.into();
            let vol = try_send(&proxy, |p| p.vol_absolute(&player, to)).await?;

            print!("{vol}");
            courtesy_line!();
        },
        ClientCommand::SwitchCurrent { to, no_play } => {
            let switch_playing = !no_play;
            try_send(&proxy, |p| p.switch_current(&to, switch_playing)).await?;
        },
    }

    Ok(())
}

async fn try_send<
    'a,
    T: 'a,
    F: Fn(&'a T) -> FR,
    FR: Future<Output = zbus::fdo::Result<R>> + 'a,
    R,
>(
    with: &'a Timeout<T>,
    call: F,
) -> Result<R> {
    const TRIES: u32 = 5;
    const DELAY_MILLIS: u64 = 5;

    let mut i = 0;

    loop {
        match with.try_run(Duration::from_secs(2), &call).await {
            Err(e) if i < TRIES => warn!("Request failed: {e}"),
            r => break r.context("Unable to contact empress server"),
        }

        i += 1;
        info!("Retry attempt {i}");

        tokio::time::sleep(Duration::from_millis(DELAY_MILLIS << i)).await;
        assert!(i <= TRIES);
    }
}
