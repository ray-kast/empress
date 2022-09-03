use std::{
    collections::HashMap,
    future::Future,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{anyhow, Context};
use futures_util::{FutureExt, StreamExt, TryFutureExt};
use log::{debug, trace, warn};
use tokio::{
    select,
    sync::{mpsc, RwLock},
};
use zbus::{zvariant::Value, Connection, Proxy};

use super::{
    method_err, mpris, mpris::player::PlaybackStatus, NowPlayingResponse, Player, PlayerMap,
    ZResult,
};
use crate::{MethodId, Offset, PlayerOpts, Result};

#[derive(Clone)]
#[repr(transparent)]
pub(super) struct Server(Arc<Inner>);

struct Inner {
    players: RwLock<PlayerMap>,
    stop_prop_changed: mpsc::Sender<()>,
}

impl Server {
    pub async fn new(conn: Arc<Connection>) -> Result<Server> {
        let players = RwLock::new(PlayerMap::new());

        let proxy = Proxy::new(
            &*conn,
            "org.freedesktop.DBus.Properties",
            &*mpris::ENTRY_PATH,
            "org.freedesktop.DBus.Properties",
        )
        .await
        .context("failed to create properties proxy")?;

        let (stop_prop_changed, mut stop_rx) = mpsc::channel(1);
        let mut evts = proxy.receive_signal("PropertiesChanged").await?;

        let this = Self(Arc::new(Inner {
            players,
            stop_prop_changed,
        }));

        tokio::spawn({
            let this = this.clone();
            let conn = Arc::clone(&conn);

            async move {
                'main: while let Some(evt) = select! {
                    evt = evts.next() => evt,
                    _ = stop_rx.recv() => None,
                } {
                    if evt
                        .interface()
                        .map_or(false, |e| e != *mpris::player::INTERFACE)
                    {
                        debug!(
                            "Ignoring PropertiesChanged for interface {:?}",
                            evt.interface()
                        );

                        continue;
                    }

                    debug!("Pending background scan...");

                    loop {
                        match select!(
                        opt = evts.next() => opt.map(|_| false),
                        () = tokio::time::sleep(Duration::from_millis(200)) => Some(true),
                        _ = stop_rx.recv() => None,
                        ) {
                            Some(true) => break,
                            Some(false) => (),
                            None => break 'main,
                        }
                    }

                    match this.scan(&*conn, true).await {
                        Ok(()) => (),
                        Err(e) => warn!("Background scan failed: {:?}", e),
                    }
                }
            }
        });

        this.scan(conn, false).await?;

        Ok(this)
    }

    async fn scan(&self, conn: impl std::borrow::Borrow<Connection>, force: bool) -> Result {
        let conn = conn.borrow();
        if force {
            debug!("Running full scan...");
        } else {
            trace!("Running quick scan...");
        }

        let proxy = Proxy::new(
            conn,
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus",
            "org.freedesktop.DBus",
        )
        .await
        .context("failed to create root D-Bus proxy")?;

        let now = Instant::now();
        let (names,): (Vec<String>,) = proxy
            .call("ListNames", &())
            .await
            .context("failed to call ListNames")?;

        PlayerMap::inform(
            &self.0.players,
            force,
            names
                .into_iter()
                .filter(|n| n.starts_with(mpris::BUS_NAME.as_str()))
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()
                .context("failed to parse player list")?,
            |n| Player::new(now, n.clone(), conn),
        )
        .await;

        trace!("Scan completed.");

        Ok(())
    }

    async fn process_player_with<
        F: Fn(Player) -> FR,
        FR: Future<Output = Result<Option<(Player, T)>>>,
        T,
    >(
        &self,
        f: F,
    ) -> Result<Option<T>> {
        let mut players = self.0.players.write().await;
        let mut patch = Err(None);

        for player in players.iter_active() {
            match f(player.clone()).await {
                Ok(Some(next)) => {
                    patch = Ok(Some(next));
                    break;
                },
                Ok(None) => patch = Ok(None),
                Err(e) => {
                    warn!("processing player failed: {:?}", e);
                    patch = patch.map_err(|_| Some(()));
                },
            }
        }

        if let Some((next, ret)) = patch
            .or_else(|e| e.map_or(Ok(None), |()| Err(anyhow!("all players failed to process"))))?
        {
            players.put(next);

            return Ok(Some(ret));
        }

        Ok(None)
    }

    async fn peek_player_with<F: Fn(Player) -> FR, FR: Future<Output = Result<Option<T>>>, T>(
        &self,
        f: F,
    ) -> Result<Option<T>> {
        let players = self.0.players.read().await;
        let mut default = Err(None);

        for player in players.iter_active() {
            match f(player.clone()).await {
                ok @ Ok(Some(_)) => return ok,
                Ok(None) => default = Ok(None),
                Err(e) => {
                    warn!("peeking player failed: {:?}", e);
                    default = default.map_err(|_| Some(()));
                },
            }
        }

        default.or_else(|e| e.map_or(Ok(None), |()| Err(anyhow!("all players failed to peek"))))
    }

    async fn process_player<F: Fn(Player) -> FR, FR: Future<Output = Result<Option<Player>>>>(
        &self,
        f: F,
    ) -> Result<bool> {
        self.process_player_with(|p| f(p).map_ok(|p| p.map(|p| (p, ()))))
            .await
            .map(|o| matches!(o, Some(())))
    }

    async fn handle_method<
        T,
        F: FnOnce() -> FR,
        FR: Future<Output = Result<T>>,
        E: std::fmt::Display + Sync,
    >(
        &self,
        conn: &Connection,
        id: MethodId,
        f: F,
        msg: E,
    ) -> ZResult<T> {
        self.scan(conn, false)
            .await
            .map_err(|e| method_err(id, e, "failed to scan for players"))?;

        f().await.map_err(|e| method_err(id, e, msg))
    }
}

// TODO: unit test this
#[zbus::dbus_interface(name = "net.ryan_s.Empress1.Daemon")]
impl Server {
    pub async fn list_players(
        &self,
        #[zbus(connection)] conn: &Connection,
    ) -> zbus::fdo::Result<(Vec<(String, String)>,)> {
        self.handle_method(
            conn,
            MethodId::ListPlayers,
            || async {
                Ok((self
                    .0
                    .players
                    .read()
                    .await
                    .iter_all()
                    .filter_map(|p| {
                        p.bus
                            .strip_prefix(mpris::BUS_NAME.as_str())
                            .and_then(|s| s.strip_prefix('.'))
                            .map(|s| (s.into(), p.status.to_string()))
                    })
                    .collect(),))
            },
            "failed to list players",
        )
        .await
    }

    pub async fn now_playing(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> zbus::fdo::Result<NowPlayingResponse> {
        self.handle_method(
            conn,
            MethodId::NowPlaying,
            || {
                self.peek_player_with(|p| async move {
                    let meta = p.metadata(conn).await?;

                    let has_track = meta
                        .get(mpris::track_list::ATTR_TRACKID)
                        .map_or(false, |v| match v {
                            Value::ObjectPath(p) => *p != mpris::track_list::NO_TRACK.as_ref(),
                            _ => false,
                        });

                    let bus = p
                        .bus
                        .strip_prefix(mpris::BUS_NAME.as_str())
                        .and_then(|s| s.strip_prefix('.'))
                        .map_or_else(String::new, Into::into);
                    let ident = p.identity(conn).await?;

                    // Properties that should go into the map regardless of if we have a track
                    let extra_props: [(String, Value); 2] = [
                        (crate::metadata::PLAYER_BUS.into(), bus.into()),
                        (crate::metadata::PLAYER_IDENTITY.into(), ident.into()),
                    ];

                    Ok(Some(if has_track {
                        let mut meta: HashMap<_, _> = meta.into_iter().chain(extra_props).collect();

                        let pos = p.position(conn).await.ok();

                        if let Some(pos) = pos {
                            meta.insert(crate::metadata::POSITION.into(), pos.into());
                        }

                        (meta, p.status)
                    } else {
                        (extra_props.into_iter().collect(), p.status)
                    }))
                })
                .map_ok(|ok| {
                    let (map, status) =
                        ok.unwrap_or_else(|| (HashMap::new(), PlaybackStatus::Stopped));

                    (map, status.to_string())
                })
            },
            "failed to get current track info",
        )
        .await
    }

    pub async fn next(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> zbus::fdo::Result<()> {
        self.handle_method(
            conn,
            MethodId::Next,
            || self.process_player(|p| p.try_next(conn)).map_ok(|_| ()),
            "failed to skip forward on a player",
        )
        .await
    }

    pub async fn prev(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> zbus::fdo::Result<()> {
        self.handle_method(
            conn,
            MethodId::Previous,
            || self.process_player(|p| p.try_previous(conn)).map_ok(|_| ()),
            "failed to skip backward on a player",
        )
        .await
    }

    pub async fn pause(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> zbus::fdo::Result<()> {
        self.handle_method(
            conn,
            MethodId::Pause,
            || self.process_player(|p| p.try_pause(conn)).map_ok(|_| ()),
            "failed to pause a player",
        )
        .await
    }

    pub async fn play_pause(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> zbus::fdo::Result<()> {
        self.handle_method(
            conn,
            MethodId::PlayPause,
            || {
                self.process_player(|p| p.try_play_pause(conn))
                    .map_ok(|_| ())
            },
            "failed to play or pause a player",
        )
        .await
    }

    pub async fn stop(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> zbus::fdo::Result<()> {
        self.handle_method(
            conn,
            MethodId::Stop,
            || self.process_player(|p| p.try_stop(conn)).map_ok(|_| ()),
            "failed to stop a player",
        )
        .await
    }

    pub async fn play(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> zbus::fdo::Result<()> {
        self.handle_method(
            conn,
            MethodId::Stop,
            || self.process_player(|p| p.try_play(conn)).map_ok(|_| ()),
            "failed to play a player",
        )
        .await
    }

    pub async fn seek_relative(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
        to: f64,
    ) -> zbus::fdo::Result<(f64,)> {
        self.handle_method(
            conn,
            MethodId::SeekRelative,
            || {
                self.process_player_with(|p| p.try_seek(conn, Offset::Relative(to)))
                    .map(|p| {
                        p?.ok_or_else(|| anyhow!("no players available to seek"))
                            .map(|p| (p,))
                    })
            },
            "failed to seek a player",
        )
        .await
    }

    pub async fn seek_absolute(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
        to: f64,
    ) -> zbus::fdo::Result<(f64,)> {
        self.handle_method(
            conn,
            MethodId::SeekAbsolute,
            || {
                self.process_player_with(|p| p.try_seek(conn, Offset::Absolute(to)))
                    .map(|p| {
                        p?.ok_or_else(|| anyhow!("no players available to seek"))
                            .map(|p| (p,))
                    })
            },
            "failed to seek a player",
        )
        .await
    }

    pub async fn vol_relative(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
        to: f64,
    ) -> zbus::fdo::Result<(f64,)> {
        self.handle_method(
            conn,
            MethodId::VolRelative,
            || {
                self.process_player_with(|p| p.try_set_volume(conn, Offset::Relative(to)))
                    .map(|p| {
                        p?.ok_or_else(|| anyhow!("no players available to get/adjust volume"))
                            .map(|p| (p,))
                    })
            },
            "failed to get/adjust a player's volume",
        )
        .await
    }

    pub async fn vol_absolute(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
        to: f64,
    ) -> zbus::fdo::Result<(f64,)> {
        self.handle_method(
            conn,
            MethodId::VolAbsolute,
            || {
                self.process_player_with(|p| p.try_set_volume(conn, Offset::Absolute(to)))
                    .map(|p| {
                        p?.ok_or_else(|| anyhow!("no players available to set volume"))
                            .map(|p| (p,))
                    })
            },
            "failed to set a player's volume",
        )
        .await
    }

    pub async fn switch_current(
        &self,
        #[zbus(connection)] conn: &Connection,
        to: &str,
        switch_playing: bool,
    ) -> zbus::fdo::Result<()> {
        self.handle_method(
            conn,
            MethodId::SwitchCurrent,
            || async {
                let bus = format!("{}.{}", *mpris::BUS_NAME, to)
                    .try_into()
                    .map_err(|s| anyhow!("{:?} is not a valid bus name", s))?;

                let curr = match self.0.players.write().await.remove(&bus) {
                    Some(c) => c,
                    None => return Err(anyhow!("no players stored with the given bus name")),
                };

                let mut curr = if switch_playing {
                    let mut put = vec![];

                    for player in self.0.players.read().await.iter_all() {
                        if player.playback_status(conn).await? == PlaybackStatus::Playing
                            && player.can_pause(conn).await?
                        {
                            put.push(player.bus.clone());
                        }
                    }

                    let mut players = self.0.players.write().await;

                    for bus in put {
                        let ply = players.remove(&bus).unwrap();

                        ply.pause(conn)
                            .await
                            .context("failed to pause another player")?;
                    }

                    curr.play(conn)
                        .await
                        .context("failed to play selected player")?
                } else {
                    curr
                };

                curr.last_update = Instant::now();
                self.0.players.write().await.put(curr);

                Ok(())
            },
            "failed to switch the current player",
        )
        .await
    }
}

// impl Drop for Server {
//     fn drop(&mut self) {
//         tokio::runtime::Builder::new_current_thread()
//             .build()
//             .unwrap()
//             .block_on(self.conn.remove_match(self.prop_changed.token()))
//             .expect("removing PropertiesChanged listener failed");
//     }
// }
