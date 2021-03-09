use std::{
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::Context;
use dbus::{
    message::MatchRule,
    nonblock::{
        stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged, MsgMatch, Proxy, SyncConnection,
    },
    MethodErr,
};
use log::{debug, warn};
use tokio::{
    select,
    sync::{mpsc, RwLock},
};

use super::{method_err, mpris, Player, PlayerMap};
use crate::{ControlMethodId, ExtraMethodId, Result};

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

    async fn process_player<
        F: Fn(Player) -> FR,
        FR: std::future::Future<Output = Result<Option<Player>>>,
    >(
        &self,
        f: F,
    ) -> Result<bool> {
        let mut players = self.players.write().await;
        let mut patch = None;

        // TODO: use drain_filter or something less stupid than this
        for player in players.iter() {
            if let Some(next) = f(player.clone())
                .await
                .context("callback failed in process_player")?
            {
                patch = Some(next);
                break;
            }
        }

        if let Some(next) = patch {
            players.put(next);

            return Ok(true);
        }

        return Ok(false);
    }

    pub async fn handle_list_players(&self) -> Result<(Vec<(String, String)>,), MethodErr> {
        self.scan(false)
            .await
            .map_err(|e| method_err(ExtraMethodId::ListPlayers, e, "failed to scan for players"))?;

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
    }

    pub async fn handle_control(&self, id: ControlMethodId) -> Result<(), MethodErr> {
        self.scan(false)
            .await
            .map_err(|e| method_err(id, e, "failed to scan for players"))?;

        match id {
            ControlMethodId::Next => {
                self.process_player(|p| p.try_next(&*self.conn))
                    .await
                    .map_err(|e| method_err(id, e, "attempting to go next on a player failed"))?;
            },
            ControlMethodId::Previous => {
                self.process_player(|p| p.try_previous(&*self.conn))
                    .await
                    .map_err(|e| {
                        method_err(id, e, "attempting to go previous on a player failed")
                    })?;
            },
            ControlMethodId::Pause => {
                self.process_player(|p| p.try_pause(&*self.conn))
                    .await
                    .map_err(|e| method_err(id, e, "attempting to pause a player failed"))?;
            },
            ControlMethodId::PlayPause => {
                self.process_player(|p| p.try_play_pause(&*self.conn))
                    .await
                    .map_err(|e| method_err(id, e, "attempting to play/pause a player failed"))?;
            },
            ControlMethodId::Stop => {
                self.process_player(|p| p.try_stop(&*self.conn))
                    .await
                    .map_err(|e| method_err(id, e, "attempting to stop a player failed"))?;
            },
            ControlMethodId::Play => {
                self.process_player(|p| p.try_play(&*self.conn))
                    .await
                    .map_err(|e| method_err(id, e, "attempting to play a player failed"))?;
            },
        }

        Ok(())
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
