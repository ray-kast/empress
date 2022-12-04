use std::{collections::HashMap, future::Future, mem, sync::Arc, time::Instant};

use anyhow::{anyhow, Context};
use futures_util::{stream, FutureExt, StreamExt, TryFutureExt, TryStreamExt};
use log::{debug, info, trace, warn};
use tokio::{
    select,
    sync::{mpsc, RwLock},
};
use zbus::{
    fdo::{self, DBusProxy},
    names::{BusName, OwnedInterfaceName, UniqueName, WellKnownName},
    zvariant::{OwnedValue, Str, Value},
    Connection, MatchRule, Message,
};

use super::{
    method_err, mpris, mpris::player::PlaybackStatus, OwnedNowPlayingResponse, Player, PlayerList,
    PlayerMap, ZResult,
};
use crate::{Offset, PlayerOpts, Result};

struct SignalMatcher<'a> {
    dbus: DBusProxy<'a>,
    match_rule: Option<MatchRule<'static>>,
}

impl<'a> Drop for SignalMatcher<'a> {
    fn drop(&mut self) {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(async move {
                let Some(mr) = mem::take(&mut self.match_rule) else {
                    return Ok(());
                };

                match self.dbus.remove_match_rule(mr).await {
                    Err(fdo::Error::ZBus(zbus::Error::Io(e)))
                        if e.kind() == std::io::ErrorKind::BrokenPipe =>
                    {
                        // TODO: this is most likely because of the construction
                        //       of a new runtime - need to look into marshaling
                        //       this block onto an existing runtime
                        warn!("Socket hung up before the signal listener was destroyed");
                        Ok(())
                    },
                    r => r,
                }
            })
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
    fn filter_player_name(name: BusName) -> Option<WellKnownName> {
        match name {
            BusName::WellKnown(w) if w.starts_with(mpris::BUS_NAME.as_str()) => Some(w),
            _ => None,
        }
    }

    fn filter_prop_changed(msg: &Message) -> Option<(HashMap<String, Value>, Vec<String>)> {
        let iface = msg.interface();
        let memb = msg.member();

        if iface.map_or(true, |i| i != "org.freedesktop.DBus.Properties")
            || memb.map_or(true, |m| m != "PropertiesChanged")
        {
            return None;
        }

        let (iface, changed, invalidated) =
            match msg.body::<(OwnedInterfaceName, HashMap<String, Value>, Vec<String>)>() {
                Ok(b) => b,
                Err(e) => {
                    debug!("Failed to deserialize possible PropertiesChanged event: {e}");

                    return None;
                },
            };

        if iface != mpris::player::INTERFACE.as_ref() {
            return None;
        }

        Some((changed, invalidated))
    }

    pub async fn new(conn: Connection) -> Result<Self> {
        let (stop_prop_changed, stop_rx) = mpsc::channel(1);

        let this = Self {
            shared: Arc::new(Shared {
                players: RwLock::new(PlayerMap::new()),
            }),
            dbus: DBusProxy::new(&conn)
                .await
                .context("Failed to create D-Bus proxy")?,
            stop_prop_changed,
        };

        this.clone()
            .spawn_background_scanner(conn.clone(), stop_rx)
            .await?;
        this.scan(conn, false).await?;

        Ok(this)
    }

    async fn spawn_background_scanner(
        self,
        conn: Connection,
        mut stop_rx: mpsc::Receiver<()>,
    ) -> Result<tokio::task::JoinHandle<()>> {
        // TODO: can this be narrowed down further?
        let mr = MatchRule::builder()
            .msg_type(zbus::MessageType::Signal)
            .interface("org.freedesktop.DBus.Properties")
            .unwrap()
            .member("PropertiesChanged")
            .unwrap()
            .build();
        self.dbus
            .add_match_rule(mr.clone())
            .await
            .context("Failed to add message match rule")?;

        let matcher = SignalMatcher {
            dbus: self.dbus.clone(),
            match_rule: Some(mr),
        };
        let mut msgs = zbus::MessageStream::from(conn.clone());

        let mut owner_changed = self
            .dbus
            .receive_name_owner_changed()
            .await
            .context("Failed to create NameOwnerChanged listener")?;

        Ok(tokio::spawn(async move {
            enum Event {
                Message(Result<Arc<Message>, zbus::Error>),
                OwnerChanged(fdo::NameOwnerChanged),
            }

            while let Some(evt) = select! {
                msg = msgs.next() => msg.map(Event::Message),
                chng = owner_changed.next() => chng.map(Event::OwnerChanged),
                _ = stop_rx.recv() => None,
            } {
                match evt {
                    Event::Message(Ok(m)) => {
                        if let Some(evt) = Self::filter_prop_changed(&m) {
                            match self.handle_property_changed(&m, evt).await {
                                Ok(true) => (),
                                Ok(false) => continue,
                                Err(e) => {
                                    warn!("Failed to process PropertyChanged signal: {e:?}");
                                    continue;
                                },
                            }
                        } else {
                            continue;
                        }
                    },
                    Event::Message(Err(e)) => {
                        warn!("Received a D-Bus error: {:?}", anyhow::Error::from(e));
                        continue;
                    },
                    Event::OwnerChanged(chng) => {
                        match self.handle_name_owner_changed(&conn, chng).await {
                            Ok(true) => (),
                            Ok(false) => continue,
                            Err(e) => {
                                warn!("Failed to process NameOwnerChanged signal: {e:?}");
                                continue;
                            },
                        }
                    },
                }

                debug!("Player list:{}", {
                    let mut s = String::new();

                    for player in self.shared.players.read().await.iter_all() {
                        use std::fmt::Write;
                        write!(
                            s,
                            "\n - {:?} ({:?} at {:?})",
                            player.bus().as_str(),
                            player.status(),
                            player.last_update(),
                        )
                        .unwrap();
                    }

                    s
                });
            }

            // TODO: consider panicking if there are extant references to the
            //       server at this point
            info!("Background scanner terminated");

            mem::drop(matcher);
        }))
    }

    #[inline]
    async fn handle_property_changed(
        &self,
        msg: &Message,
        (changed, invalidated): (HashMap<String, Value<'_>>, Vec<String>),
    ) -> Result<bool> {
        let header = msg.header().context("Invalid message header")?;

        let sender = header
            .sender()
            .context("Invalid message sender")?
            .ok_or_else(|| anyhow!("Message had no sender"))?;

        let players = self.shared.players.read().await;

        // TODO: unsure how to avoid this to_owned()
        let sender = players
            .resolve(&sender.to_owned())
            .ok_or_else(|| anyhow!("Unexpected bus name"))?
            .to_owned();

        mem::drop(players);

        let mut ret = false;

        if !changed.is_empty() {
            trace!("properties on {:?} changed: {changed:?}", sender.as_str());

            for (name, val) in changed {
                // TODO: find a more elegant way to do this
                #[allow(clippy::single_match)]
                match &*name {
                    "PlaybackStatus" => {
                        if let Some((mut players, mut player, val)) = async {
                            let mut players = self.shared.players.write().await;
                            let player = players.remove(&sender)?;

                            let val: PlaybackStatus = val
                                .downcast_ref::<str>()?
                                .parse()
                                .map_err(|e| warn!("Failed to parse playback status: {e}"))
                                .ok()?;

                            Some((players, player, val))
                        }
                        .await
                        {
                            player.update_status(val);
                            players.put(player);

                            ret = true;
                        }
                    },
                    _ => (),
                }
            }
        }

        if !invalidated.is_empty() {
            trace!(
                "properties on {:?} invalidated: {invalidated:?}",
                sender.as_str(),
            );

            for name in invalidated {
                // TODO: find a more elegant way to do this
                #[allow(clippy::single_match)]
                match &*name {
                    "PlaybackStatus" => {
                        // TODO: there are several occurrences of the remove-
                        //       update-put pattern in the code, these could be
                        //       simplified to a single "mutate" method
                        if let Some((mut players, mut player, status)) = async {
                            let mut players = self.shared.players.write().await;
                            let player = players.remove(&sender)?;

                            let status = player
                                .playback_status()
                                .await
                                .map_err(|e| warn!("Failed to get player status: {e}"))
                                .ok()?;

                            Some((players, player, status))
                        }
                        .await
                        {
                            player.update_status(status);
                            players.put(player);

                            ret = true;
                        }
                    },
                    _ => (),
                }
            }
        }

        Ok(ret)
    }

    #[inline]
    async fn handle_name_owner_changed(
        &self,
        conn: &Connection,
        chng: fdo::NameOwnerChanged,
    ) -> Result<bool> {
        let args = chng
            .args()
            .context("Failed to parse NameOwnerChanged arguments")?;

        let Some(name) = Self::filter_player_name(args.name) else {
            return Ok(false);
        };

        if args.old_owner.is_none() && args.new_owner.is_none() {
            return Ok(false);
        }

        let mut players = self.shared.players.write().await;

        if let Some(old) = Option::<UniqueName>::from(args.old_owner) {
            // TODO: unsure how to avoid this to_owned()
            players.remove_owner(&old.to_owned());

            trace!("{:?} lost {:?}", old.as_str(), name.as_str());
        }

        if let Some(new) = Option::<UniqueName>::from(args.new_owner) {
            players
                .put_owner(conn, Instant::now(), new.to_owned(), name.to_owned())
                .await?;

            trace!("{:?} acquired {:?}", new.as_str(), name.as_str());
        }

        Ok(true)
    }

    async fn get_player_names(&self) -> Result<impl Iterator<Item = WellKnownName<'static>>> {
        Ok(self
            .dbus
            .list_names()
            .await
            .context("Failed to list bus names")?
            .into_iter()
            .filter_map(|n| Self::filter_player_name(n.into_inner())))
    }

    async fn get_player_name_map(
        &self,
    ) -> Result<HashMap<UniqueName<'static>, WellKnownName<'static>>> {
        self.get_player_names()
            .await?
            .map(|n| async {
                let uniq = self
                    .dbus
                    .get_name_owner(BusName::WellKnown(n.as_ref()))
                    .await?;

                trace!("{:?} owns {:?}", uniq.as_str(), n.as_str());

                Ok((uniq.into_inner(), n))
            })
            .collect::<stream::FuturesOrdered<_>>()
            .try_collect()
            .await
    }

    async fn scan(&self, conn: impl std::borrow::Borrow<Connection>, force: bool) -> Result {
        let conn = conn.borrow();
        if force {
            debug!("Running full scan...");
        } else {
            trace!("Running quick scan...");
        }

        let now = Instant::now();

        self.shared
            .players
            .write()
            .await
            .inform(conn, now, force, self.get_player_name_map().await?)
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
        opts: PlayerOpts,
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
                    warn!("Processing player failed: {e:?}");
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
        opts: PlayerOpts,
        f: F,
    ) -> Result<Option<T>> {
        let players = self.shared.players.read().await;
        let mut default = Err(None);

        for player in players.iter_active() {
            match f(player.clone()).await {
                ok @ Ok(Some(_)) => return ok,
                Ok(None) => default = Ok(None),
                Err(e) => {
                    warn!("Peeking player failed: {e:?}");
                    default = default.map_err(|_| Some(()));
                },
            }
        }

        default.or_else(|e| e.map_or(Ok(None), |()| Err(anyhow!("All players failed to peek"))))
    }

    async fn process_player<F: Fn(Player) -> FR, FR: Future<Output = Result<Option<Player>>>>(
        &self,
        opts: PlayerOpts,
        f: F,
    ) -> Result<bool> {
        self.process_player_with(opts, |p| f(p).map_ok(|p| p.map(|p| (p, ()))))
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
        _conn: &Connection,
        f: F,
        msg: E,
    ) -> ZResult<T> {
        // TODO: clean this and the unused parameter up
        // self.scan(conn, false)
        //     .await
        //     .map_err(|e| method_err(e, "Failed to scan for players"))?;

        f().await.map_err(|e| method_err(e, msg))
    }
}

#[zbus::dbus_interface(name = "net.ryan_s.Empress2.Daemon")]
impl Server {
    pub async fn list_players(
        &self,
        #[zbus(connection)] conn: &Connection,
    ) -> fdo::Result<PlayerList> {
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
                            .map(|s| (s.into(), p.status()))
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
        opts: PlayerOpts,
    ) -> fdo::Result<OwnedNowPlayingResponse> {
        self.handle_method(
            conn,
            || {
                self.peek_player_with(opts, |p| async move {
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

                    (map, status)
                })
            },
            "Failed to get current track info",
        )
        .await
    }

    pub async fn next(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(opts, Player::try_next).map_ok(|_| ()),
            "Failed to skip forward on a player",
        )
        .await
    }

    pub async fn prev(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || {
                self.process_player(opts, Player::try_previous)
                    .map_ok(|_| ())
            },
            "Failed to skip backward on a player",
        )
        .await
    }

    pub async fn pause(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(opts, Player::try_pause).map_ok(|_| ()),
            "Failed to pause a player",
        )
        .await
    }

    pub async fn play_pause(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || {
                self.process_player(opts, Player::try_play_pause)
                    .map_ok(|_| ())
            },
            "Failed to play or pause a player",
        )
        .await
    }

    pub async fn stop(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(opts, Player::try_stop).map_ok(|_| ()),
            "Failed to stop a player",
        )
        .await
    }

    pub async fn play(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
    ) -> fdo::Result<()> {
        self.handle_method(
            conn,
            || self.process_player(opts, Player::try_play).map_ok(|_| ()),
            "Failed to play a player",
        )
        .await
    }

    pub async fn seek_relative(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
        to: f64,
    ) -> fdo::Result<f64> {
        self.handle_method(
            conn,
            || {
                self.process_player_with(opts, |p| p.try_seek(Offset::Relative(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to seek")))
            },
            "Failed to seek a player",
        )
        .await
    }

    pub async fn seek_absolute(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
        to: f64,
    ) -> fdo::Result<f64> {
        self.handle_method(
            conn,
            || {
                self.process_player_with(opts, |p| p.try_seek(Offset::Absolute(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to seek")))
            },
            "Failed to seek a player",
        )
        .await
    }

    pub async fn vol_relative(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
        to: f64,
    ) -> fdo::Result<f64> {
        self.handle_method(
            conn,
            || {
                self.process_player_with(opts, |p| p.try_set_volume(Offset::Relative(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to get/adjust volume")))
            },
            "Failed to get/adjust a player's volume",
        )
        .await
    }

    pub async fn vol_absolute(
        &self,
        #[zbus(connection)] conn: &Connection,
        opts: PlayerOpts,
        to: f64,
    ) -> fdo::Result<f64> {
        self.handle_method(
            conn,
            || {
                self.process_player_with(opts, |p| p.try_set_volume(Offset::Absolute(to)))
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
                let bus = WellKnownName::try_from(format!("{}.{to}", *mpris::BUS_NAME))
                    .map_err(|s| anyhow!("{s:?} is not a valid bus name"))?;

                let Some(curr) = self.shared.players.write().await.remove(&bus) else {
                    return Err(anyhow!("No players stored with the given bus name"));
                };

                let mut curr = if switch_playing {
                    let mut put = vec![];
                    let mut players = self.shared.players.write().await;

                    for player in players.iter_all() {
                        if player.playback_status().await? == PlaybackStatus::Playing
                            && player.can_pause().await?
                        {
                            put.push(player.bus().to_owned());
                        }
                    }

                    for bus in put {
                        let player = players.remove(&bus).unwrap();
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
