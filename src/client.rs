use std::{io, time::Duration};

use anyhow::{Context, Error};
use log::{info, trace, warn};
use serde::Serialize;
use zbus::zvariant;

use crate::{
    format,
    interface::{
        metadata, mpris, DaemonProxy, NowPlayingResponse, PlaybackStatus, SERVER_NAME, SERVER_PATH,
    },
    ClientCommand, Offset, Result,
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
    status: PlaybackStatus,
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
            .remove(metadata::PLAYER_BUS)
            .and_then(|v| v.downcast_ref::<str>().map(ToOwned::to_owned));
        let id = map
            .remove(metadata::PLAYER_IDENTITY)
            .and_then(|v| v.downcast_ref::<str>().map(ToOwned::to_owned));
        let title = map
            .remove(mpris::track_list::ATTR_TITLE)
            .and_then(|v| v.downcast_ref::<str>().map(ToOwned::to_owned));
        let artist = map.remove(mpris::track_list::ATTR_ARTIST).and_then(|v| {
            v.downcast_ref::<zvariant::Array>().map(|i| {
                i.iter()
                    .filter_map(|v| v.downcast_ref::<str>().map(ToOwned::to_owned))
                    .collect::<Vec<_>>()
            })
        });
        let album = map
            .remove(mpris::track_list::ATTR_ALBUM)
            .and_then(|v| v.downcast_ref::<str>().map(ToOwned::to_owned));
        let length = map
            .remove(mpris::track_list::ATTR_LENGTH)
            .and_then(|v| v.downcast_ref().copied());
        let position = map
            .remove(metadata::POSITION)
            .and_then(|v| v.downcast_ref().copied());

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

enum Continuation {
    ListPlayers(Vec<(String, PlaybackStatus)>),
    NowPlaying {
        format: Option<String>,
        resp: NowPlayingResponse,
    },
    Seeked(f64),
    VolumeSet(f64),
}

// Hack to make handle_command less annoying to write
trait Continue {
    fn k(self) -> zbus::Result<Option<Continuation>>;
}

impl Continue for zbus::Result<()> {
    fn k(self) -> zbus::Result<Option<Continuation>> { self.map(|()| None) }
}

async fn handle_command(
    cmd: ClientCommand,
    proxy: DaemonProxy<'_>,
) -> Result<Option<Continuation>> {
    const MAX_TRIES: usize = 5;

    let mut res = Err(anyhow::anyhow!("Unreachable code detected"));

    for i in 0..MAX_TRIES {
        // TODO: Can this clone be removed without making the code a nightmare?
        res = match cmd.clone() {
            ClientCommand::ListPlayers => proxy
                .list_players()
                .await
                .map(|list| Some(Continuation::ListPlayers(list))),
            ClientCommand::Next(search) => proxy.next(search.into()).await.k(),
            ClientCommand::NowPlaying { search, format } => proxy
                .now_playing(search.into())
                .await
                .map(|resp| Some(Continuation::NowPlaying { format, resp })),
            ClientCommand::Pause(search) => proxy.pause(search.into()).await.k(),
            ClientCommand::Play(search) => proxy.play(search.into()).await.k(),
            ClientCommand::PlayPause(search) => proxy.play_pause(search.into()).await.k(),
            ClientCommand::Previous(search) => proxy.previous(search.into()).await.k(),
            ClientCommand::Seek { search, pos } => match pos {
                Offset::Absolute(to) => proxy.seek_absolute(search.into(), to).await,
                Offset::Relative(by) => proxy.seek_relative(search.into(), by).await,
            }
            .map(|pos| Some(Continuation::Seeked(pos))),
            ClientCommand::Stop(search) => proxy.stop(search.into()).await.k(),
            ClientCommand::SwitchCurrent { to, no_play } => {
                proxy.switch_current(to, !no_play).await.k()
            },
            ClientCommand::Volume { search, vol } => match vol {
                Offset::Absolute(to) => proxy.vol_absolute(search.into(), to).await,
                Offset::Relative(by) => proxy.vol_relative(search.into(), by).await,
            }
            .map(|vol| Some(Continuation::VolumeSet(vol))),
        }
        .context("Failed to contact D-Bus daemon");

        if let Err(ref e) = res {
            warn!("{:?}", e);
        } else {
            return res;
        }

        info!("Retrying (attempt {})...", i + 1);

        tokio::time::sleep(Duration::from_millis(20)).await;
    }

    assert!(res.is_err());

    res
}

pub(super) async fn run(cmd: ClientCommand) -> Result {
    let conn = zbus::Connection::session()
        .await
        .context("Failed to connect to D-Bus")?;

    let proxy = DaemonProxy::builder(&conn)
        .destination(&*SERVER_NAME)?
        .path(&*SERVER_PATH)?
        .build()
        .await
        .context("Failed to create D-Bus daemon proxy")?;

    let k = match handle_command(cmd, proxy).await? {
        Some(c) => c,
        None => return Ok(()),
    };

    match k {
        Continuation::ListPlayers(list) => {
            for (player, status) in list {
                println!("{}\t{}", player, status);
            }
        },
        Continuation::NowPlaying { format, resp } => {
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
        Continuation::Seeked(pos) => print!("{}", pos),
        Continuation::VolumeSet(vol) => print!("{}", vol),
    }

    Ok(())
}
