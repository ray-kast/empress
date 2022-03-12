use std::{
    collections::HashMap,
    future::Future,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{anyhow, Context};
use dbus::{
    arg::{ArgType, RefArg, Variant},
    message::MatchRule,
    nonblock::{
        stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged, MsgMatch, Proxy, SyncConnection,
    },
    strings::{BusName, Path},
};
use futures::prelude::*;
use log::{debug, trace, warn};
use tokio::{
    select,
    sync::{mpsc, RwLock},
};

use super::{
    method_err, mpris, mpris::player::PlaybackStatus, MethodResult, NowPlayingResponse, Player,
    PlayerMap,
};
use crate::{MethodId, Offset, PlayerOpts, Result};

pub(super) struct Server {
    conn: Arc<SyncConnection>,
    players: RwLock<PlayerMap>,
    prop_changed: MsgMatch,
}

impl Server {
    pub async fn new(conn: Arc<SyncConnection>) -> Result<Arc<Server>> {
        let players = RwLock::new(PlayerMap::new());

        let mut mr = MatchRule::new_signal("org.freedesktop.DBus.Properties", "PropertiesChanged");
        mr.path = Some(mpris::ENTRY_PATH.clone());
        mr.path_is_namespace = false;

        let (scan_tx, mut scan_rx) = mpsc::channel(1);
        let prop_changed = conn
            .add_match(mr)
            .await
            .context("failed to listen for property changes")?
            .cb(move |_, changed: PropertiesPropertiesChanged| {
                if changed.interface_name.as_str() != &**mpris::player::INTERFACE {
                    debug!(
                        "Ignoring PropertiesChanged for interface {:?}",
                        changed.interface_name
                    );

                    return true;
                }

                scan_tx.try_send(()).ok();
                true
            });

        let ret = Arc::new(Self {
            conn,
            players,
            prop_changed,
        });

        let self_1 = ret.clone();
        tokio::spawn(async move {
            'main: while let Some(()) = scan_rx.recv().await {
                debug!("Pending background scan...");

                loop {
                    match select!(
                        opt = scan_rx.recv() => opt.map(|()| false),
                        () = tokio::time::sleep(Duration::from_millis(200)) => Some(true),
                    ) {
                        Some(true) => break,
                        Some(false) => (),
                        None => break 'main,
                    }
                }

                match self_1.scan(true).await {
                    Ok(()) => (),
                    Err(e) => warn!("Background scan failed: {:?}", e),
                }
            }
        });

        ret.scan(false).await?;

        Ok(ret)
    }

    async fn scan(&self, force: bool) -> Result {
        if force {
            debug!("Running full scan...");
        } else {
            trace!("Running quick scan...");
        }

        let proxy = Proxy::new(
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus",
            Duration::from_secs(2),
            self.conn.clone(),
        );

        let now = Instant::now();
        let (names,): (Vec<String>,) = proxy
            .method_call("org.freedesktop.DBus", "ListNames", ())
            .await
            .context("failed to call ListNames")?;

        PlayerMap::inform(
            &self.players,
            force,
            names
                .into_iter()
                .filter(|n| n.starts_with(&**mpris::BUS_NAME))
                .map(Into::into)
                .collect(),
            |n| Player::new(now, n.clone(), &*self.conn),
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
        let mut players = self.players.write().await;
        let mut patch = Err(None);

        for player in players.iter() {
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
        let players = self.players.read().await;
        let mut default = Err(None);

        for player in players.iter() {
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
        E: std::fmt::Display,
    >(
        &self,
        id: MethodId,
        f: F,
        msg: E,
    ) -> MethodResult<T> {
        self.scan(false)
            .await
            .map_err(|e| method_err(id, e, "failed to scan for players"))?;

        f().await.map_err(|e| method_err(id, e, msg))
    }

    pub async fn list_players(&self) -> MethodResult<(Vec<(String, String)>,)> {
        self.handle_method(
            MethodId::ListPlayers,
            || async {
                Ok((self
                    .players
                    .read()
                    .await
                    .iter()
                    .filter_map(|p| {
                        p.bus
                            .strip_prefix(&**mpris::BUS_NAME)
                            .and_then(|s| s.strip_prefix('.'))
                            .map(|s| (s.into(), p.status.to_string()))
                    })
                    .collect(),))
            },
            "failed to list players",
        )
        .await
    }

    pub async fn now_playing(&self) -> MethodResult<NowPlayingResponse> {
        self.handle_method(
            MethodId::NowPlaying,
            || {
                self.peek_player_with(|p| async move {
                    let meta = p.metadata(&*self.conn).await?;

                    let has_track =
                        meta.get(mpris::track_list::ATTR_TRACKID)
                            .map_or(false, |Variant(v)| {
                                !(v.arg_type() == ArgType::ObjectPath
                                    && v.as_str()
                                        .and_then(|s| {
                                            Path::new(s)
                                                .map_err(|e| {
                                                    warn!("Failed to parse MPRIS trackid: {:?}", e);
                                                })
                                                .ok()
                                        })
                                        .map_or(false, |p| p == *mpris::track_list::NO_TRACK))
                            });

                    Ok(Some(if has_track {
                        let mut meta: HashMap<_, _> = meta
                            .into_iter()
                            .map(|(k, Variant(v))| (k, Variant(v.box_clone())))
                            .collect();

                        let bus = p
                            .bus
                            .strip_prefix(&**mpris::BUS_NAME)
                            .and_then(|s| s.strip_prefix('.'))
                            .map_or_else(String::new, Into::into);
                        let ident = p.identity(&*self.conn).await?;

                        meta.insert(crate::metadata::PLAYER_BUS.into(), Variant(Box::new(bus)));

                        meta.insert(
                            crate::metadata::PLAYER_IDENTITY.into(),
                            Variant(Box::new(ident)),
                        );

                        (meta, p.status)
                    } else {
                        (HashMap::new(), p.status)
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

    pub async fn next(&self, PlayerOpts {}: PlayerOpts) -> MethodResult {
        self.handle_method(
            MethodId::Next,
            || {
                self.process_player(|p| p.try_next(&*self.conn))
                    .map_ok(|_| ())
            },
            "failed to skip forward on a player",
        )
        .await
    }

    pub async fn prev(&self, PlayerOpts {}: PlayerOpts) -> MethodResult {
        self.handle_method(
            MethodId::Previous,
            || {
                self.process_player(|p| p.try_previous(&*self.conn))
                    .map_ok(|_| ())
            },
            "failed to skip backward on a player",
        )
        .await
    }

    pub async fn pause(&self, PlayerOpts {}: PlayerOpts) -> MethodResult {
        self.handle_method(
            MethodId::Pause,
            || {
                self.process_player(|p| p.try_pause(&*self.conn))
                    .map_ok(|_| ())
            },
            "failed to pause a player",
        )
        .await
    }

    pub async fn play_pause(&self, PlayerOpts {}: PlayerOpts) -> MethodResult {
        self.handle_method(
            MethodId::PlayPause,
            || {
                self.process_player(|p| p.try_play_pause(&*self.conn))
                    .map_ok(|_| ())
            },
            "failed to play or pause a player",
        )
        .await
    }

    pub async fn stop(&self, PlayerOpts {}: PlayerOpts) -> MethodResult {
        self.handle_method(
            MethodId::Stop,
            || {
                self.process_player(|p| p.try_stop(&*self.conn))
                    .map_ok(|_| ())
            },
            "failed to stop a player",
        )
        .await
    }

    pub async fn play(&self, PlayerOpts {}: PlayerOpts) -> MethodResult {
        self.handle_method(
            MethodId::Stop,
            || {
                self.process_player(|p| p.try_play(&*self.conn))
                    .map_ok(|_| ())
            },
            "failed to play a player",
        )
        .await
    }

    pub async fn seek_relative(&self, PlayerOpts {}: PlayerOpts, to: f64) -> MethodResult<(f64,)> {
        self.handle_method(
            MethodId::SeekRelative,
            || {
                self.process_player_with(|p| p.try_seek(&*self.conn, Offset::Relative(to)))
                    .map(|p| {
                        p?.ok_or_else(|| anyhow!("no players available to seek"))
                            .map(|p| (p,))
                    })
            },
            "failed to seek a player",
        )
        .await
    }

    pub async fn seek_absolute(&self, PlayerOpts {}: PlayerOpts, to: f64) -> MethodResult<(f64,)> {
        self.handle_method(
            MethodId::SeekAbsolute,
            || {
                self.process_player_with(|p| p.try_seek(&*self.conn, Offset::Absolute(to)))
                    .map(|p| {
                        p?.ok_or_else(|| anyhow!("no players available to seek"))
                            .map(|p| (p,))
                    })
            },
            "failed to seek a player",
        )
        .await
    }

    pub async fn vol_relative(&self, PlayerOpts {}: PlayerOpts, to: f64) -> MethodResult<(f64,)> {
        self.handle_method(
            MethodId::VolRelative,
            || {
                self.process_player_with(|p| p.try_set_volume(&*self.conn, Offset::Relative(to)))
                    .map(|p| {
                        p?.ok_or_else(|| anyhow!("no players available to get/adjust volume"))
                            .map(|p| (p,))
                    })
            },
            "failed to get/adjust a player's volume",
        )
        .await
    }

    pub async fn vol_absolute(&self, PlayerOpts {}: PlayerOpts, to: f64) -> MethodResult<(f64,)> {
        self.handle_method(
            MethodId::VolAbsolute,
            || {
                self.process_player_with(|p| p.try_set_volume(&*self.conn, Offset::Absolute(to)))
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
        to: impl std::fmt::Display,
        switch_playing: bool,
    ) -> MethodResult<()> {
        self.handle_method(
            MethodId::SwitchCurrent,
            || async {
                let bus = BusName::new(format!("{}.{}", *mpris::BUS_NAME, to))
                    .map_err(|s| anyhow!("{:?} is not a valid bus name", s))?;

                let curr = match self.players.write().await.remove(&bus) {
                    Some(c) => c,
                    None => return Err(anyhow!("no players stored with the given bus name")),
                };

                let mut curr = if switch_playing {
                    let mut put = vec![];
                    let conn = &*self.conn;

                    for player in self.players.read().await.iter() {
                        if player.playback_status(conn).await? == PlaybackStatus::Playing
                            && player.can_pause(conn).await?
                        {
                            put.push(player.bus.clone());
                        }
                    }

                    let mut players = self.players.write().await;

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
                self.players.write().await.put(curr);

                Ok(())
            },
            "failed to switch the current player",
        )
        .await
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(self.conn.remove_match(self.prop_changed.token()))
            .expect("removing PropertiesChanged listener failed");
    }
}
