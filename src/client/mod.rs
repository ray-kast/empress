use std::{future::Future, io, io::IsTerminal, time::Duration};

use anyhow::{Context, Error};
use futures_util::StreamExt;
use log::{info, trace, warn};
use serde::Serialize;
use zbus::connection;

use self::proxy::EmpressProxy;
use crate::{
    format,
    server::{
        self, mpris, mpris::player::PlaybackStatus, MatchPlayer, PlayerStatus, PlayerStatusKind,
    },
    timeout::Timeout,
    ClientCommand, Offset, PlayerOpts, Result, SERVER_NAME,
};

mod proxy;

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct NowPlayingPlayer {
    bus: Option<String>,
    id: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct NowPlayingResult {
    status: PlaybackStatus,
    player: NowPlayingPlayer,
    title: Option<String>,
    artist: Option<Vec<String>>,
    album: Option<String>,
    length: Option<i64>,
    position: Option<i64>,
}

impl TryFrom<PlayerStatus> for NowPlayingResult {
    type Error = Error;

    fn try_from(status: PlayerStatus) -> Result<Self> {
        let PlayerStatus {
            kind,
            bus,
            ident,
            status,
            position,
            mut metadata,
        } = status;

        let (bus, id, position) = match kind {
            PlayerStatusKind::NoPlayer => (None, None, None),
            PlayerStatusKind::NoPosition => (Some(bus), Some(ident), None),
            PlayerStatusKind::Default => (Some(bus), Some(ident), Some(position)),
        };

        let title = metadata
            .remove(mpris::track_list::ATTR_TITLE)
            .and_then(|v| v.try_into().ok());
        let artist = metadata
            .remove(mpris::track_list::ATTR_ARTIST)
            .and_then(|v| v.try_into().ok());
        let album = metadata
            .remove(mpris::track_list::ATTR_ALBUM)
            .and_then(|v| v.try_into().ok());
        let length = metadata
            .remove(mpris::track_list::ATTR_LENGTH)
            .and_then(|v| v.try_into().ok());

        Ok(Self {
            status,
            player: NowPlayingPlayer { bus, id },
            title,
            artist,
            album,
            length,
            position,
        })
    }
}

impl MatchPlayer for NowPlayingResult {
    fn bus(&self) -> &str { self.player.bus.as_ref().map_or("", |s| s) }

    fn status(&self) -> PlaybackStatus { self.status }
}

macro_rules! courtesy_line {
    () => {
        if std::io::stdout().is_terminal() {
            println!();
        }
    };
}

#[allow(clippy::too_many_lines)]
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
        } => now_playing(proxy, player, format, watch).await?,
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

async fn now_playing(
    proxy: Timeout<EmpressProxy<'_>>,
    player: PlayerOpts,
    format: Option<String>,
    watch: bool,
) -> Result {
    let player = player.into();

    let status = try_send(&proxy, |p| async {
        if player == server::PlayerOpts::default() {
            p.now_playing().await
        } else {
            p.player_status(&player).await
        }
    })
    .await?;

    let format = format.as_deref();
    let mut last = print_now_playing(status, format, |_| true)?.1;

    if watch {
        println!();

        let player = player.build().context("Invalid player filter options")?;
        let mut stream = proxy
            .run(
                Duration::from_secs(1),
                EmpressProxy::receive_now_playing_changed,
            )
            .await
            .context("Error getting now-playing status")?;

        while let Some(s) = stream.next().await {
            let val = s.get().await.context("Error parsing property value")?;

            trace!("Full now-playing response: {val:?}");

            let (skip, val) = print_now_playing(val, format, |v| *v != last && player.is_match(v))?;
            if skip {
                continue;
            }

            println!();

            last = val;
        }
    } else {
        courtesy_line!();
    }

    Ok(())
}

fn print_now_playing(
    resp: PlayerStatus,
    format: Option<&str>,
    f: impl FnOnce(&NowPlayingResult) -> bool,
) -> Result<(bool, NowPlayingResult)> {
    trace!("Full now-playing response: {resp:?}");

    let resp: NowPlayingResult = resp.try_into()?;

    if !f(&resp) {
        return Ok((true, resp));
    }

    if let Some(format) = format {
        print!("{}", format::eval(format, &resp)?);
    } else {
        serde_json::to_writer(io::stdout(), &resp)?;
    }

    Ok((false, resp))
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
    const MAX_TRIES: usize = 5;

    let mut i = 0;

    loop {
        match with.try_run(Duration::from_secs(2), &call).await {
            Err(e) if i < MAX_TRIES => warn!("Request failed: {e}"),
            r => break r.context("Unable to contact empress server"),
        }

        i += 1;
        info!("Retry attempt {i}");

        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}
