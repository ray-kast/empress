use std::{io, sync::Arc, time::Duration};

use anyhow::{Context, Error};
use dbus::{
    arg::{AppendAll, ReadAll, RefArg, Variant},
    nonblock::{Proxy, SyncConnection},
};
use dbus_tokio::connection;
use log::{info, trace, warn};
use serde::Serialize;
use tokio::{select, sync::oneshot, task};

use crate::{
    format,
    server::{mpris, mpris::player::PlaybackStatus, NowPlayingResponse},
    ClientCommand, MethodId, PlayerOpts, Result, INTERFACE_NAME, SERVER_NAME, SERVER_PATH,
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

impl TryFrom<NowPlayingResponse> for NowPlayingResult {
    type Error = Error;

    fn try_from((mut map, status): NowPlayingResponse) -> Result<Self> {
        let bus = map
            .remove(crate::metadata::PLAYER_BUS)
            .and_then(|Variant(v)| v.as_str().map(ToOwned::to_owned));
        let id = map
            .remove(crate::metadata::PLAYER_IDENTITY)
            .and_then(|Variant(v)| v.as_str().map(ToOwned::to_owned));
        let title = map
            .remove(mpris::track_list::ATTR_TITLE)
            .and_then(|Variant(v)| v.as_str().map(ToOwned::to_owned));
        let artist = map
            .remove(mpris::track_list::ATTR_ARTIST)
            .and_then(|Variant(v)| {
                v.as_iter().map(|i| {
                    i.filter_map(|v| v.as_str().map(ToOwned::to_owned))
                        .collect::<Vec<_>>()
                })
            });
        let album = map
            .remove(mpris::track_list::ATTR_ALBUM)
            .and_then(|Variant(v)| v.as_str().map(ToOwned::to_owned));
        let length = map
            .remove(mpris::track_list::ATTR_LENGTH)
            .and_then(|Variant(v)| v.as_i64());
        let position = map
            .remove(crate::metadata::POSITION)
            .and_then(|Variant(v)| v.as_i64());

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
    let (res, conn) = connection::new_session_sync().context("failed to connect to D-Bus")?;
    let (close_tx, close_rx) = oneshot::channel();

    task::spawn(async {
        close_tx
            .send(Error::from(res.await).context("D-Bus disconnected"))
            .ok();
    });

    let run = async move {
        let proxy = Proxy::new(&*SERVER_NAME, &*SERVER_PATH, Duration::from_secs(2), conn);

        let id = cmd.id();

        match cmd {
            ClientCommand::Next(opts)
            | ClientCommand::Previous(opts)
            | ClientCommand::Pause(opts)
            | ClientCommand::PlayPause(opts)
            | ClientCommand::Stop(opts)
            | ClientCommand::Play(opts) => {
                let PlayerOpts {} = opts;
                try_send(&proxy, id, ()).await?;
            },
            ClientCommand::ListPlayers => {
                let (players,): (Vec<(String, String)>,) = try_send(&proxy, id, ()).await?;

                for (player, status) in players {
                    println!("{}\t{}", player, status);
                }
            },
            ClientCommand::NowPlaying {
                player: PlayerOpts {},
                format,
            } => {
                let resp: NowPlayingResponse = try_send(&proxy, id, ()).await?;

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
            ClientCommand::Seek {
                player: PlayerOpts {},
                to,
            } => {
                try_send(&proxy, id, (to.offset(),)).await?;
            },
            ClientCommand::Volume {
                player: PlayerOpts {},
                vol,
            } => {
                let (vol,): (f64,) = try_send(&proxy, id, (vol.offset(),)).await?;

                print!("{}", vol);

                if atty::is(atty::Stream::Stdout) {
                    println!();
                }
            },
            ClientCommand::SwitchCurrent { to, no_play } => {
                let switch_playing = !no_play;
                try_send(&proxy, id, (to, switch_playing)).await?;
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

async fn try_send<R: ReadAll + 'static, A: AppendAll + Clone>(
    proxy: &Proxy<'_, Arc<SyncConnection>>,
    method: MethodId,
    args: A,
) -> Result<R> {
    const MAX_TRIES: usize = 5;

    let method = method.to_string();
    let mut i = 0;

    loop {
        match proxy
            .method_call(&*INTERFACE_NAME, &method, args.clone())
            .await
        {
            Err(e) if i < MAX_TRIES => warn!("Request failed: {}", e),
            r => break r.context("failed to contact empress server"),
        }

        i += 1;
        info!("Retry attempt {}", i);

        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}
