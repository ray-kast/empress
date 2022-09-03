use std::{io, time::Duration};

use anyhow::{Context, Error};
use log::{info, trace, warn};
use serde::{de::DeserializeOwned, Serialize};
use zbus::{
    zvariant::{DynamicType, Type},
    ConnectionBuilder, Proxy,
};

use crate::{
    format,
    server::{mpris, mpris::player::PlaybackStatus, OwnedNowPlayingResponse},
    ClientCommand, MethodId, Result, INTERFACE_NAME, SERVER_NAME, SERVER_PATH,
};

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct NowPlayingPlayer {
    bus: Option<String>,
    id: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct NowPlayingResult {
    status: String,
    player: NowPlayingPlayer,
    title: Option<String>,
    artist: Option<Vec<String>>,
    album: Option<String>,
    length: Option<i64>,
    position: Option<i64>,
}

impl TryFrom<OwnedNowPlayingResponse> for NowPlayingResult {
    type Error = Error;

    fn try_from((mut map, status): OwnedNowPlayingResponse) -> Result<Self> {
        let bus = map
            .remove(crate::metadata::PLAYER_BUS)
            .and_then(|v| v.try_into().ok());
        let id = map
            .remove(crate::metadata::PLAYER_IDENTITY)
            .and_then(|v| v.try_into().ok());
        let title = map
            .remove(mpris::track_list::ATTR_TITLE)
            .and_then(|v| v.try_into().ok());
        let artist = map
            .remove(mpris::track_list::ATTR_ARTIST)
            .and_then(|v| v.try_into().ok());
        let album = map
            .remove(mpris::track_list::ATTR_ALBUM)
            .and_then(|v| v.try_into().ok());
        let length = map
            .remove(mpris::track_list::ATTR_LENGTH)
            .and_then(|v| v.try_into().ok());
        let position = map
            .remove(crate::metadata::POSITION)
            .and_then(|v| v.try_into().ok());

        let _: PlaybackStatus = status.parse()?;

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
        .context("Failed to create session connection builder")?
        .build()
        .await
        .context("Failed to connect to D-Bus")?;

    // TODO: declare timeout
    let proxy = Proxy::new(&conn, &*SERVER_NAME, &*SERVER_PATH, &*INTERFACE_NAME)
        .await
        .context("Failed to create server proxy")?;

    let id = cmd.id();

    match cmd {
        ClientCommand::Next(opts)
        | ClientCommand::Previous(opts)
        | ClientCommand::Pause(opts)
        | ClientCommand::PlayPause(opts)
        | ClientCommand::Stop(opts)
        | ClientCommand::Play(opts) => {
            try_send(&proxy, id, &(opts,)).await?;
        },
        ClientCommand::ListPlayers => {
            let (players,): (Vec<(String, String)>,) = try_send(&proxy, id, &()).await?;

            for (player, status) in players {
                println!("{}\t{}", player, status);
            }
        },
        ClientCommand::NowPlaying { player, format } => {
            let resp: OwnedNowPlayingResponse = try_send(&proxy, id, &(player,)).await?;

            trace!("Full now-playing response: {:?}", resp);

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
        ClientCommand::Seek { player, to } => {
            try_send(&proxy, id, &(player, to.offset())).await?;
        },
        ClientCommand::Volume { player, vol } => {
            let (vol,): (f64,) = try_send(&proxy, id, &(player, vol.offset())).await?;

            print!("{}", vol);

            if atty::is(atty::Stream::Stdout) {
                println!();
            }
        },
        ClientCommand::SwitchCurrent { to, no_play } => {
            let switch_playing = !no_play;
            try_send(&proxy, id, &(to, switch_playing)).await?;
        },
    }

    Ok(())
}

async fn try_send<R: Type + DeserializeOwned + 'static, A: DynamicType + Serialize + Clone>(
    proxy: &Proxy<'_>,
    method: MethodId,
    args: &A,
) -> Result<R> {
    const MAX_TRIES: usize = 5;

    let method = method.to_string();
    let mut i = 0;

    loop {
        match proxy.call(&*method, args).await {
            Err(e) if i < MAX_TRIES => warn!("Request failed: {}", e),
            r => break r.context("Failed to contact empress server"),
        }

        i += 1;
        info!("Retry attempt {}", i);

        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}
