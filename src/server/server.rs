use std::{collections::HashMap, future::Future, mem, sync::Arc, time::Instant};

use anyhow::{anyhow, Context};
use futures_util::{stream, FutureExt, StreamExt, TryFutureExt, TryStreamExt};
use log::{debug, info, trace, warn};
use tokio::{
    select,
    sync::{mpsc, RwLock},
    task::JoinHandle,
};
use zbus::{
    fdo::{self, DBusProxy},
    names::{BusName, OwnedInterfaceName, UniqueName, WellKnownName},
    zvariant::Value,
    Connection, MatchRule, Message,
};

use super::{
    method_err, mpris, mpris::player::PlaybackStatus, Player, PlayerList, PlayerMap, PlayerStatus,
    PlayerStatusKind, ZResult,
};
use crate::{Offset, PlayerOpts, Result};

struct SignalMatcher<'a> {
    dbus: DBusProxy<'a>,
    match_rule: Option<MatchRule<'static>>,
}

impl<'a> SignalMatcher<'a> {
    pub async fn shutdown(&mut self) -> Result {
        self.dbus
            .remove_match_rule(
                self.match_rule.take().ok_or_else(|| {
                    anyhow!("shutdown() called on already-closed signal matcher!")
                })?,
            )
            .await
            .context("Error removing signal match rule")
    }
}

impl<'a> Drop for SignalMatcher<'a> {
    fn drop(&mut self) {
        // TODO: async drop wen eta son
        assert!(
            self.match_rule.is_none(),
            "Signal matcher dropped without calling .shutdown()!"
        );
    }
}

pub(super) struct Server {
    inner: ServerInner,
    #[allow(dead_code)] // This signals the scanner by getting dropped
    stop_scanner: mpsc::Sender<()>,
}

#[derive(Clone)]
pub(super) struct ServerInner {
    shared: Arc<Shared>,
    dbus: DBusProxy<'static>,
}

struct Shared {
    players: RwLock<PlayerMap>,
}

impl Server {
    pub async fn new(conn: Connection) -> Result<(Self, impl Future<Output = Result>)> {
        let (stop_scanner, stop_rx) = mpsc::channel(1);

        let inner = ServerInner {
            shared: Arc::new(Shared {
                players: RwLock::new(PlayerMap::new()),
            }),
            dbus: DBusProxy::new(&conn)
                .await
                .context("Error creating D-Bus proxy")?,
        };

        let scanner_handle = inner
            .clone()
            .spawn_background_scanner(conn.clone(), stop_rx)
            .await?;
        inner.scan(conn, false).await?;

        Ok((
            Self {
                inner,
                stop_scanner,
            },
            scanner_handle.map_err(|e| anyhow!("Background scanner panicked: {e:?}")),
        ))
    }
}

impl std::ops::Deref for Server {
    type Target = ServerInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl ServerInner {
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
                    debug!("Error deserializing possible PropertiesChanged event: {e}");

                    return None;
                },
            };

        if iface != mpris::player::INTERFACE.as_ref() {
            return None;
        }

        Some((changed, invalidated))
    }

    async fn spawn_background_scanner(
        self,
        conn: Connection,
        mut stop_rx: mpsc::Receiver<()>,
    ) -> Result<JoinHandle<()>> {
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
            .context("Error adding message match rule")?;

        let mut matcher = SignalMatcher {
            dbus: self.dbus.clone(),
            match_rule: Some(mr),
        };
        let mut msgs = zbus::MessageStream::from(conn.clone());

        let mut owner_changed = self
            .dbus
            .receive_name_owner_changed()
            .await
            .context("Error creating NameOwnerChanged listener")?;

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
                                    warn!("Error processing PropertyChanged signal: {e:?}");
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
                                warn!("Error processing NameOwnerChanged signal: {e:?}");
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

            matcher
                .shutdown()
                .await
                .map_err(|e| warn!("Error unregistering background scan matcher: {e:?}"))
                .ok();

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
                                .map_err(|e| warn!("Error parsing playback status: {e}"))
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
                                .map_err(|e| warn!("Error getting player status: {e}"))
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
            .context("Error parsing NameOwnerChanged arguments")?;

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
            .context("Error listing bus names")?
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
                    warn!("Error processing player: {e:?}");
                    patch = patch.map_err(|_| Some(()));
                },
            }
        }

        if let Some((next, ret)) = patch.or_else(|e| {
            e.map_or(Ok(None), |()| {
                Err(anyhow!("No players could be processed without errors"))
            })
        })? {
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
                    warn!("Error peeking player: {e:?}");
                    default = default.map_err(|_| Some(()));
                },
            }
        }

        default.or_else(|e| {
            e.map_or(Ok(None), |()| {
                Err(anyhow!("No players could be peeked without errors"))
            })
        })
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
        f: F,
        msg: E,
    ) -> ZResult<T> {
        f().await.map_err(|e| method_err(e, msg))
    }
}

#[zbus::dbus_interface(name = "net.ryan_s.Empress2.Daemon")]
impl Server {
    pub async fn list_players(&self) -> fdo::Result<PlayerList> {
        self.handle_method(
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
            "Error listing players",
        )
        .await
    }

    #[dbus_interface(signal)]
    pub async fn player_changed(
        &self,
        ctx: &zbus::SignalContext<'_>,
        change: &PlayerStatus,
    ) -> zbus::Result<()>;

    // #[dbus_interface(property)]
    // pub async fn now_playing(&self) -> PlayerStatus {
    //     todo!()
    // }

    pub async fn player_status(&self, opts: PlayerOpts) -> fdo::Result<PlayerStatus> {
        self.handle_method(
            || {
                self.peek_player_with(opts, |p| async move {
                    let metadata = p.metadata().await?;

                    let has_track =
                        metadata
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
                    let status = p.status();
                    let position = if has_track {
                        p.position().await.ok()
                    } else {
                        None
                    };

                    Ok(Some(PlayerStatus {
                        kind: if position.is_some() {
                            PlayerStatusKind::Default
                        } else {
                            PlayerStatusKind::NoPosition
                        },
                        bus,
                        ident,
                        status,
                        position: position.unwrap_or(0),
                        metadata,
                    }))
                })
                .map_ok(Option::unwrap_or_default)
            },
            "Error getting player status",
        )
        .await
    }

    pub async fn next(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            || self.process_player(opts, Player::try_next).map_ok(|_| ()),
            "Error skipping player forward",
        )
        .await
    }

    pub async fn prev(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            || {
                self.process_player(opts, Player::try_previous)
                    .map_ok(|_| ())
            },
            "Error skipping player backward",
        )
        .await
    }

    pub async fn pause(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            || self.process_player(opts, Player::try_pause).map_ok(|_| ()),
            "Error pausing player",
        )
        .await
    }

    pub async fn play_pause(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            || {
                self.process_player(opts, Player::try_play_pause)
                    .map_ok(|_| ())
            },
            "Error playing/pausing player",
        )
        .await
    }

    pub async fn stop(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            || self.process_player(opts, Player::try_stop).map_ok(|_| ()),
            "Error stopping player",
        )
        .await
    }

    pub async fn play(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            || self.process_player(opts, Player::try_play).map_ok(|_| ()),
            "Error playing player",
        )
        .await
    }

    pub async fn seek_relative(&self, opts: PlayerOpts, to: f64) -> fdo::Result<f64> {
        self.handle_method(
            || {
                self.process_player_with(opts, |p| p.try_seek(Offset::Relative(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to seek")))
            },
            "Error seeking player",
        )
        .await
    }

    pub async fn seek_absolute(&self, opts: PlayerOpts, to: f64) -> fdo::Result<f64> {
        self.handle_method(
            || {
                self.process_player_with(opts, |p| p.try_seek(Offset::Absolute(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to seek")))
            },
            "Error seeking player",
        )
        .await
    }

    pub async fn vol_relative(&self, opts: PlayerOpts, to: f64) -> fdo::Result<f64> {
        self.handle_method(
            || {
                self.process_player_with(opts, |p| p.try_set_volume(Offset::Relative(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to get/adjust volume")))
            },
            "Error getting/adjusting player volume",
        )
        .await
    }

    pub async fn vol_absolute(&self, opts: PlayerOpts, to: f64) -> fdo::Result<f64> {
        self.handle_method(
            || {
                self.process_player_with(opts, |p| p.try_set_volume(Offset::Absolute(to)))
                    .map(|p| p?.ok_or_else(|| anyhow!("No players available to set volume")))
            },
            "Error setting player volume",
        )
        .await
    }

    pub async fn switch_current(&self, to: &str, switch_playing: bool) -> fdo::Result<()> {
        self.handle_method(
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
                            .context("Error pausing an unselected player")?;

                        players.put(player);
                    }

                    curr.play().await.context("Error playing selected player")?
                } else {
                    curr
                };

                curr.force_update();
                self.shared.players.write().await.put(curr);

                Ok(())
            },
            "Error switching current player",
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
