use std::{future::Future, io, time::Duration};

use anyhow::{Context, Error};
use log::{info, trace, warn};
use serde::Serialize;
use zbus::ConnectionBuilder;

use crate::{
    format,
    server::{mpris, mpris::player::PlaybackStatus, PlayerStatus, PlayerStatusKind},
    ClientCommand, Offset, Result, SERVER_NAME,
};

mod proxy;

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct NowPlayingPlayer {
    bus: Option<String>,
    id: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
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
        let PlayerStatus { kind, bus, ident, status, position, mut metadata } = status;

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

pub(super) async fn run(cmd: ClientCommand) -> Result {
    let conn = ConnectionBuilder::session()
        .context("Error creatihng session connection builder")?
        .build()
        .await
        .context("Error connecting to D-Bus")?;

    // TODO: declare timeout
    let proxy = proxy::EmpressProxy::builder(&conn)
        .destination(&*SERVER_NAME)
        .context("Error setting empress proxy destination")?
        .build()
        .await
        .context("Error building server proxy")?;

    match cmd {
        ClientCommand::Next(opts) => {
            try_send(|| proxy.next(&opts)).await?;
        },
        ClientCommand::Previous(opts) => {
            try_send(|| proxy.prev(&opts)).await?;
        },
        ClientCommand::Pause(opts) => {
            try_send(|| proxy.pause(&opts)).await?;
        },
        ClientCommand::PlayPause(opts) => {
            try_send(|| proxy.play_pause(&opts)).await?;
        },
        ClientCommand::Stop(opts) => {
            try_send(|| proxy.stop(&opts)).await?;
        },
        ClientCommand::Play(opts) => {
            try_send(|| proxy.play(&opts)).await?;
        },
        ClientCommand::ListPlayers => {
            let players = try_send(|| proxy.list_players()).await?;

            for (player, status) in players {
                println!("{player}\t{status}");
            }
        },
        ClientCommand::NowPlaying { player, format } => {
            let resp = try_send(|| proxy.player_status(&player)).await?;

            trace!("Full now-playing response: {resp:?}");

            let resp: NowPlayingResult = resp.try_into()?;

            if let Some(format) = format {
                print!("{}", format::eval(format, resp)?);
            } else {
                serde_json::to_writer(io::stdout(), &resp)?;
            }

            if atty::is(atty::Stream::Stdout) {
                println!();
            }
        },
        ClientCommand::Seek {
            player,
            to: Offset::Relative(to),
        } => {
            try_send(|| proxy.seek_relative(&player, to)).await?;
        },
        ClientCommand::Seek {
            player,
            to: Offset::Absolute(to),
        } => {
            try_send(|| proxy.seek_absolute(&player, to)).await?;
        },
        ClientCommand::Volume {
            player,
            vol: Offset::Relative(to),
        } => {
            let vol = try_send(|| proxy.vol_relative(&player, to)).await?;

            print!("{vol}");

            if atty::is(atty::Stream::Stdout) {
                println!();
            }
        },
        ClientCommand::Volume {
            player,
            vol: Offset::Absolute(to),
        } => {
            let vol = try_send(|| proxy.vol_absolute(&player, to)).await?;

            print!("{vol}");

            if atty::is(atty::Stream::Stdout) {
                println!();
            }
        },
        ClientCommand::SwitchCurrent { to, no_play } => {
            let switch_playing = !no_play;
            try_send(|| proxy.switch_current(&to, switch_playing)).await?;
        },
    }

    Ok(())
}

async fn try_send<F: Future<Output = zbus::fdo::Result<R>>, R>(call: impl Fn() -> F) -> Result<R> {
    const MAX_TRIES: usize = 5;

    let mut i = 0;

    loop {
        match call().await {
            Err(e) if i < MAX_TRIES => warn!("Request failed: {e}"),
            r => break r.context("Unable to contact empress server"),
        }

        i += 1;
        info!("Retry attempt {i}");

        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}
