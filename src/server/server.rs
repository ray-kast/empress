use std::{
    collections::HashMap,
    future::Future,
    mem,
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
use zbus::{
    fdo::{self, DBusProxy},
    names::{OwnedBusName, OwnedInterfaceName},
    zvariant::{OwnedValue, Str, Value},
    Connection, Message,
};

use super::{
    method_err, mpris, mpris::player::PlaybackStatus, OwnedNowPlayingResponse, Player, PlayerMap,
    ZResult,
};
use crate::{Offset, PlayerOpts, Result};

struct SignalMatcher<'a> {
    dbus: DBusProxy<'a>,
    expr: String,
}

impl<'a> Drop for SignalMatcher<'a> {
    fn drop(&mut self) {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(async move { self.dbus.remove_match(&mem::take(&mut self.expr)).await })
            .expect("Removing signal listener failed");
    }
}

#[derive(Clone)]
pub(super) struct Server {
    shared: Arc<Shared>,
    dbus: DBusProxy<'static>,
    #[allow(dead_code)]
    stop_prop_changed: mpsc::Sender<()>,
}

struct Shared {
    players: RwLock<PlayerMap>,
}

impl Server {
    fn filter_prop_changed(msg: &Message) -> bool {
        let iface = msg.interface();
        let memb = msg.member();

        if iface.map_or(true, |i| i != "org.freedesktop.DBus.Properties")
            || memb.map_or(true, |m| m != "PropertiesChanged")
        {
            return false;
        }

        let (iface, _props, _) =
            match msg.body::<(OwnedInterfaceName, HashMap<String, Value>, Vec<String>)>() {
                Ok(b) => b,
                Err(e) => {
                    debug!(
                        "Failed to deserialize possible PropertiesChanged event: {}",
                        e
                    );

                    return false;
                },
            };

        if iface != mpris::player::INTERFACE.as_ref() {
            return false;
        }

        true
    }

    pub async fn new(conn: Connection) -> Result<Self> {
        let players = RwLock::new(PlayerMap::new());
        let (stop_prop_changed, mut stop_rx) = mpsc::channel(1);
        let dbus = DBusProxy::new(&conn)
            .await
            .context("Failed to create D-Bus proxy")?;

        let this = Self {
            shared: Arc::new(Shared { players }),
            dbus: dbus.clone(),
            stop_prop_changed,
        };

        let expr = concat!(
            "type='signal',",
            "interface='org.freedesktop.DBus.Properties',",
            "member='PropertiesChanged'"
        )
        .to_owned();
        let mut msgs = zbus::MessageStream::from(conn.clone());

        dbus.add_match(&expr)
            .await
            .context("Failed to add message match rule")?;
        let matcher = SignalMatcher { dbus, expr };

        tokio::spawn({
            let this = this.clone();
            let conn = conn.clone();

            async move {
                'main: while let Some(msg) = select! {
                    msg = msgs.next() => msg,
                    _ = stop_rx.recv() => None,
                } {
                    // TODO: kill the server
                    let msg = msg.context("Received error message").unwrap();

                    trace!("{:?}", msg);

                    if !Self::filter_prop_changed(&msg) {
                        continue;
                    }

                    debug!("Pending background scan...");

                    loop {
                        // TODO: unsure if draining messages is necessary anymore
                        match select! {
                            m = msgs.next() => m.map(|_| false),
                            () = tokio::time::sleep(Duration::from_millis(200)) => Some(true),
                            _ = stop_rx.recv() => None,
                        } {
                            Some(true) => break,
                            Some(false) => (),
                            None => break 'main,
                        }
                    }

                    match this.scan(&conn, true).await {
                        Ok(()) => (),
                        Err(e) => warn!("Background scan failed: {:?}", e),
                    }
                }

                mem::drop(matcher);
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

        let now = Instant::now();
        let names = self
            .dbus
            .list_names()
            .await
            .context("Failed to call ListNames")?;

        PlayerMap::inform(
            &self.shared.players,
            force,
            names
                .into_iter()
                .filter(|n| n.starts_with(mpris::BUS_NAME.as_str()))
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()
                .context("Failed to parse player list")?,
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
        let mut players = self.shared.players.write().await;
        let mut patch = Err(None);

        for player in players.iter_active() {
            match f(player.clone()).await {
                Ok(Some(next)) => {
                    patch = Ok(Some(next));
                    break;
                },
                Ok(None) => patch = Ok(None),
                Err(e) => {
                    warn!("Processing player failed: {:?}", e);
                    patch = patch.map_err(|_| Some(()));
                },
            }
        }

        if let Some((next, ret)) = patch
            .or_else(|e| e.map_or(Ok(None), |()| Err(anyhow!("All players failed to process"))))?
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
        let players = self.shared.players.read().await;
        let mut default = Err(None);

        for player in players.iter_active() {
            match f(player.clone()).await {
                ok @ Ok(Some(_)) => return ok,
                Ok(None) => default = Ok(None),
                Err(e) => {
                    warn!("Peeking player failed: {:?}", e);
                    default = default.map_err(|_| Some(()));
                },
            }
        }

        default.or_else(|e| e.map_or(Ok(None), |()| Err(anyhow!("All players failed to peek"))))
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
        f: F,
        msg: E,
    ) -> ZResult<T> {
        self.scan(conn, false)
            .await
            .map_err(|e| method_err(e, "Failed to scan for players"))?;

        f().await.map_err(|e| method_err(e, msg))
    }
}

#[zbus::dbus_interface(name = "net.ryan_s.Empress2.Daemon")]
impl Server {
    pub async fn list_players(
        &self,
        #[zbus(connection)] conn: &Connection,
    ) -> fdo::Result<Vec<(String, String)>> {
        self.handle_method(
            conn,
            || async {
                Ok(self
                    .shared
                    .players
                    .read()
                    .await
                    .iter_all()
                    .filter_map(|p| {
                        p.bus()
                            .strip_prefix(mpris::BUS_NAME.as_str())
                            .and_then(|s| s.strip_prefix('.'))
                            .map(|s| (s.into(), p.status().to_string()))
                    })
                    .collect())
            },
            "Failed to list players",
        )
        .await
    }

    pub async fn now_playing(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> fdo::Result<OwnedNowPlayingResponse> {
        self.handle_method(
            conn,
            || {
                self.peek_player_with(|p| async move {
                    let meta = p.metadata().await?;

                    let has_track = meta
                        .get(mpris::track_list::ATTR_TRACKID)
                        .map_or(false, |v| match v.into() {
                            Value::ObjectPath(p) => p != mpris::track_list::NO_TRACK.as_ref(),
                            _ => false,
                        });

                    let bus = p
                        .bus()
                        .strip_prefix(mpris::BUS_NAME.as_str())
                        .and_then(|s| s.strip_prefix('.'))
                        .map_or_else(String::new, Into::into);
                    let ident = p.identity().await?;

                    // Properties that should go into the map regardless of if we have a track
                    let extra_props: [(String, OwnedValue); 2] = [
                        (crate::metadata::PLAYER_BUS.into(), Str::from(bus).into()),
                        (
                            crate::metadata::PLAYER_IDENTITY.into(),
                            Str::from(ident).into(),
                        ),
                    ];

                    Ok(Some(if has_track {
                        let mut meta: HashMap<_, _> = meta.into_iter().chain(extra_props).collect();

                        let pos = p.position().await.ok();

                        if let Some(pos) = pos {
                            meta.insert(crate::metadata::POSITION.into(), pos.into());
                        }

                        (meta, p.status())
                    } else {
                        (extra_props.into_iter().collect(), p.status())
                    }))
                })
                .map_ok(|ok| {
                    let (map, status) =
                        ok.unwrap_or_else(|| (HashMap::new(), PlaybackStatus::Stopped));

                    (map, status.to_string())
                })
            },
            "Failed to get current track info",
        )
        .await
    }

    pub async fn next(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(Player::try_next).map_ok(|_| ()),
            "Failed to skip forward on a player",
        )
        .await
    }

    pub async fn prev(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(Player::try_previous).map_ok(|_| ()),
            "Failed to skip backward on a player",
        )
        .await
    }

    pub async fn pause(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(Player::try_pause).map_ok(|_| ()),
            "Failed to pause a player",
        )
        .await
    }

    pub async fn play_pause(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(Player::try_play_pause).map_ok(|_| ()),
            "Failed to play or pause a player",
        )
        .await
    }

    pub async fn stop(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(Player::try_stop).map_ok(|_| ()),
            "Failed to stop a player",
        )
        .await
    }

    pub async fn play(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(Player::try_play).map_ok(|_| ()),
            "Failed to play a player",
        )
        .await
    }

    pub async fn seek_relative(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
        to: f64,
    ) -> fdo::Result<f64> {
        self.handle_method(
            conn,
            || {
                self.process_player_with(|p| p.try_seek(Offset::Relative(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to seek")))
            },
            "Failed to seek a player",
        )
        .await
    }

    pub async fn seek_absolute(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
        to: f64,
    ) -> fdo::Result<f64> {
        self.handle_method(
            conn,
            || {
                self.process_player_with(|p| p.try_seek(Offset::Absolute(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to seek")))
            },
            "Failed to seek a player",
        )
        .await
    }

    pub async fn vol_relative(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
        to: f64,
    ) -> fdo::Result<f64> {
        self.handle_method(
            conn,
            || {
                self.process_player_with(|p| p.try_set_volume(Offset::Relative(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to get/adjust volume")))
            },
            "Failed to get/adjust a player's volume",
        )
        .await
    }

    pub async fn vol_absolute(
        &self,
        #[zbus(connection)] conn: &Connection,
        PlayerOpts {}: PlayerOpts,
        to: f64,
    ) -> fdo::Result<f64> {
        self.handle_method(
            conn,
            || {
                self.process_player_with(|p| p.try_set_volume(Offset::Absolute(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to set volume")))
            },
            "Failed to set a player's volume",
        )
        .await
    }

    pub async fn switch_current(
        &self,
        #[zbus(connection)] conn: &Connection,
        to: &str,
        switch_playing: bool,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || async {
                let bus = format!("{}.{}", *mpris::BUS_NAME, to)
                    .try_into()
                    .map_err(|s| anyhow!("{:?} is not a valid bus name", s))?;

                let curr = match self.shared.players.write().await.remove(&bus) {
                    Some(c) => c,
                    None => return Err(anyhow!("No players stored with the given bus name")),
                };

                let mut curr = if switch_playing {
                    let mut put = vec![];
                    let mut players = self.shared.players.write().await;

                    for player in players.iter_all() {
                        if player.playback_status().await? == PlaybackStatus::Playing
                            && player.can_pause().await?
                        {
                            put.push(OwnedBusName::from(player.bus().to_owned()));
                        }
                    }

                    for bus in put {
                        let player = players.remove(&bus.into()).unwrap();
                        let player = player
                            .pause()
                            .await
                            .context("Failed to pause another player")?;

                        players.put(player);
                    }

                    curr.play()
                        .await
                        .context("Failed to play selected player")?
                } else {
                    curr
                };

                curr.force_update();
                self.shared.players.write().await.put(curr);

                Ok(())
            },
            "Failed to switch the current player",
        )
        .await
    }
}

#[cfg(test)]
mod tests {
    use zbus::Interface;

    #[test]
    fn test_interface() {
        assert_eq!(super::Server::name(), crate::INTERFACE_NAME.as_ref());
    }
}
