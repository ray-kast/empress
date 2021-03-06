use std::{
    future::Future,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{anyhow, Context};
use dbus::{
    message::MatchRule,
    nonblock::{
        stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged, MsgMatch, Proxy, SyncConnection,
    },
};
use futures::prelude::*;
use log::{debug, warn};
use tokio::{
    select,
    sync::{mpsc, RwLock},
};

use super::{method_err, mpris, MethodResult, Player, PlayerMap};
use crate::{MethodId, PlayerOpts, Result, SeekPosition};

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
        debug!("Running {} scan...", if force { "full" } else { "quick" });

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
                .map(|n| n.into())
                .collect(),
            |n| Player::new(now, n.clone(), &*self.conn),
        )
        .await;

        debug!("{} scan completed", if force { "Full" } else { "Quick" });

        Ok(())
    }

    async fn process_player_with<
        F: Fn(Player) -> FR,
        FR: std::future::Future<Output = Result<Option<(Player, T)>>>,
        T,
    >(
        &self,
        f: F,
    ) -> Result<Option<T>> {
        let mut players = self.players.write().await;
        let mut patch = Err(());

        // TODO: use drain_filter or something less stupid than this
        for player in players.iter() {
            match f(player.clone()).await {
                Ok(Some(next)) => {
                    patch = Ok(Some(next));
                    break;
                },
                Ok(None) => patch = Ok(None),
                Err(e) => warn!("processing player failed: {:?}", e),
            }
        }

        if let Some((next, ret)) = patch.map_err(|()| anyhow!("all players failed to process"))? {
            players.put(next);

            return Ok(Some(ret));
        }

        return Ok(None);
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
                            .and_then(|s| s.strip_prefix("."))
                            .map(|s| (s.into(), p.status.to_string()))
                    })
                    .collect(),))
            },
            "failed to list players",
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
                self.process_player_with(|p| p.try_seek(&*self.conn, SeekPosition::Relative(to)))
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
                self.process_player_with(|p| p.try_seek(&*self.conn, SeekPosition::Absolute(to)))
                    .map(|p| {
                        p?.ok_or_else(|| anyhow!("no players available to seek"))
                            .map(|p| (p,))
                    })
            },
            "failed to seek a player",
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
