use std::{collections::HashMap, future::Future, sync::Arc, time::Instant};

use anyhow::{anyhow, ensure, Context};
use futures_util::{stream, FutureExt, StreamExt, TryFutureExt, TryStreamExt};
use log::{debug, error, trace, warn};
use tokio::{
    select,
    sync::{mpsc, RwLock},
    task::JoinHandle,
};
use zbus::{
    fdo::{self, DBusProxy},
    names::{BusName, OwnedInterfaceName, UniqueName, WellKnownName},
    object_server::SignalEmitter,
    zvariant::{ObjectPath, OwnedValue, Value},
    Connection, MatchRule, Message,
};

use super::{
    method_err,
    mpris::{self, player::PlaybackStatus},
    player::{self, Action},
    player_map::PlayerChange,
    Player, PlayerList, PlayerMap, PlayerOpts, PlayerStatus, PlayerStatusKind, ZResult,
};
use crate::{server::PlayerMatcher, Offset, Result};

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

// TODO: miserable AsyncFn workaround
trait PeekFn {
    type Output;

    fn peek(&self, player: &Player) -> impl Future<Output = Result<Option<Self::Output>>>;
}

impl<F: Fn(&Player) -> R, R: Future<Output = Result<Option<T>>>, T> PeekFn for F {
    type Output = T;

    fn peek(&self, player: &Player) -> impl Future<Output = Result<Option<Self::Output>>> {
        self(player)
    }
}

#[derive(Clone)]
pub(super) struct Server {
    shared: Arc<Shared>,
    dbus: DBusProxy<'static>,
}

struct Shared {
    players: RwLock<PlayerMap>,
}

impl Server {
    pub async fn new(
        conn: Connection,
        path: ObjectPath<'static>,
    ) -> Result<(Self, impl Future<Output = Result>)> {
        let (stop_scanner, stop_rx) = mpsc::channel(1);

        let this = Self {
            shared: Arc::new(Shared {
                players: RwLock::new(PlayerMap::new()),
            }),
            dbus: DBusProxy::new(&conn)
                .await
                .context("Error creating D-Bus proxy")?,
        };

        let ctx =
            SignalEmitter::new(&conn, path).context("Error creating scanner signal context")?;

        let scanner_handle = this.clone().spawn_background_scanner(ctx, stop_rx).await?;
        this.run_scan(conn, false, None).await?;

        Ok((this, async move {
            drop(stop_scanner);
            scanner_handle
                .await
                .map_err(|e| anyhow!("Background scanner panicked: {e:?}"))
        }))
    }

    fn filter_player_name(name: BusName) -> Option<WellKnownName> {
        match name {
            BusName::WellKnown(w) if w.starts_with(mpris::BUS_NAME.as_str()) => Some(w),
            _ => None,
        }
    }

    fn filter_prop_changed(msg: &Message) -> Option<(HashMap<String, OwnedValue>, Vec<String>)> {
        let hdr = msg.header();
        let iface = hdr.interface();
        let memb = hdr.member();

        if iface.map_or(true, |i| i != "org.freedesktop.DBus.Properties")
            || memb.map_or(true, |m| m != "PropertiesChanged")
        {
            return None;
        }

        let (iface, changed, invalidated) =
            match msg
                .body()
                .deserialize::<(OwnedInterfaceName, HashMap<String, OwnedValue>, Vec<String>)>()
            {
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
        ctx: SignalEmitter<'static>,
        stop_rx: mpsc::Receiver<()>,
    ) -> Result<JoinHandle<()>> {
        // TODO: can this be narrowed down further?
        let mr = MatchRule::builder()
            .msg_type(zbus::message::Type::Signal)
            .interface("org.freedesktop.DBus.Properties")
            .unwrap()
            .member("PropertiesChanged")
            .unwrap()
            .build();
        self.dbus
            .add_match_rule(mr.clone())
            .await
            .context("Error adding message match rule")?;

        let matcher = SignalMatcher {
            dbus: self.dbus.clone(),
            match_rule: Some(mr),
        };
        let msgs = zbus::MessageStream::from(ctx.connection().clone());

        let owner_changed = self
            .dbus
            .receive_name_owner_changed()
            .await
            .context("Error creating NameOwnerChanged listener")?;

        Ok(tokio::spawn(self.background_scanner(
            ctx,
            stop_rx,
            matcher,
            msgs,
            owner_changed,
        )))
    }

    async fn background_scanner(
        self,
        ctx: SignalEmitter<'_>,
        mut stop_rx: mpsc::Receiver<()>,
        mut matcher: SignalMatcher<'_>,
        mut msgs: zbus::MessageStream,
        mut owner_changed: zbus::fdo::NameOwnerChangedStream,
    ) {
        enum Event {
            Message(Result<Message, zbus::Error>),
            OwnerChanged(fdo::NameOwnerChanged),
        }

        let _dfr = dispose::defer(|| {
            // If the server still holds a reference to the player map then we
            // are not supposed to be here
            if Arc::strong_count(&self.shared) > 1 {
                error!("Background scanner terminated unexpectedly!");

                dispose::abort_on_panic(|| panic!("Background scanner terminated unexpectedly!"));
            }
        });

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
                    match self.handle_name_owner_changed(ctx.connection(), chng).await {
                        Ok(true) => (),
                        Ok(false) => continue,
                        Err(e) => {
                            warn!("Error processing NameOwnerChanged signal: {e:?}");
                            continue;
                        },
                    }
                },
            }

            // TODO: this may trigger false positives (and, in cases where
            //       changes are not propagated by the background scanner,
            //       false negatives)
            match self.now_playing_changed(&ctx).await {
                Ok(()) => (),
                Err(e) => {
                    error!("Error sending change event for NowPlaying: {e}");
                    break;
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

        drop(matcher);
    }

    #[inline]
    async fn handle_property_changed(
        &self,
        msg: &Message,
        (changed, invalidated): (HashMap<String, OwnedValue>, Vec<String>),
    ) -> Result<bool> {
        let header = msg.header();

        let sender = header.sender().context("Message had no sender")?;

        let players = self.shared.players.read().await;

        let sender = players
            .resolve(&sender.to_owned())
            .with_context(|| format!("Unexpected bus name {sender:?}"))?
            .to_owned();

        drop(players);

        let mut ret = false;

        if !changed.is_empty() {
            trace!("properties on {sender:?} changed: {changed:?}");

            for (name, val) in changed {
                // TODO: find a more elegant way to do this
                #[allow(clippy::single_match)]
                match &*name {
                    "PlaybackStatus" => {
                        let mut players = self.shared.players.write().await;

                        if let Some((mut player, val)) = async {
                            let player = players.get_mut(&sender)?;

                            let val: PlaybackStatus = val
                                .downcast_ref::<&str>()
                                .context("Error downcasting playback status")
                                .and_then(|s| s.parse().context("Error parsing playback status"))
                                .with_context(|| {
                                    format!(
                                        "Error handling property change of {name:?} for {sender:?}"
                                    )
                                })
                                .map_err(|e| warn!("{e:?}"))
                                .ok()?;

                            Some((player, val))
                        }
                        .await
                        {
                            player.update_status(val);
                            ret = true;
                        };
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
                        let mut players = self.shared.players.write().await;

                        if let Some((mut player, status)) = async {
                            let player = players.get_mut(&sender)?;

                            let status = player
                                .playback_status()
                                .await
                                .context("Error getting player status")
                                .with_context(|| {
                                    format!(
                                        "Error handling property invalidaton of {name:?} for \
                                         {sender:?}"
                                    )
                                })
                                .map_err(|e| warn!("{e:?}"))
                                .ok()?;

                            Some((player, status))
                        }
                        .await
                        {
                            player.update_status(status);
                            ret = true;
                        };
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

    async fn run_scan(
        &self,
        conn: impl std::borrow::Borrow<Connection>,
        force: bool,
        changes: Option<&mut Vec<PlayerChange>>,
    ) -> Result<()> {
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
            .inform(conn, now, force, self.get_player_name_map().await?, changes)
            .await;

        trace!("Scan completed.");

        Ok(())
    }

    async fn apply_action<A: Action>(
        &self,
        opts: PlayerOpts,
        action: A,
    ) -> Result<Option<A::Output>> {
        struct MatchAction<A>(PlayerMatcher, A);

        impl<A: Action> Action for MatchAction<A> {
            type Arg = A::Arg;
            type Output = A::Output;

            async fn can_run(&self, player: &Player) -> Result<Option<Self::Arg>> {
                if self.0.is_match(player) {
                    self.1.can_run(player).await
                } else {
                    Ok(None)
                }
            }

            #[inline]
            fn run(
                &self,
                player: &mut Player,
                arg: Self::Arg,
            ) -> impl Future<Output = Result<Self::Output>> {
                self.1.run(player, arg)
            }
        }

        let matcher = opts.build().context("Invalid player filter options")?;
        let mut players = self.shared.players.write().await;

        players.apply_action(MatchAction(matcher, action)).await
    }

    async fn find_map_player<F: PeekFn>(
        &self,
        opts: PlayerOpts,
        f: F,
    ) -> Result<Option<F::Output>> {
        let matcher = opts.build().context("Invalid player filter options")?;
        let players = self.shared.players.read().await;
        let mut default = Err(None);

        for player in players.iter_active() {
            if !matcher.is_match(player) {
                continue;
            }

            match f.peek(player).await {
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

    async fn handle_method<T, F: Future<Output = Result<T>>, E: std::fmt::Display + Sync>(
        &self,
        f: F,
        msg: E,
    ) -> ZResult<T> {
        f.await.map_err(|e| method_err(e, msg))
    }

    async fn peek_player_status(&self, opts: PlayerOpts) -> Result<PlayerStatus> {
        struct Peek;

        impl PeekFn for Peek {
            type Output = PlayerStatus;

            async fn peek(&self, player: &Player) -> Result<Option<Self::Output>> {
                let metadata = player.metadata().await?;

                let has_track = metadata
                    .get(mpris::track_list::ATTR_TRACKID)
                    .map_or(false, |v| match **v {
                        Value::ObjectPath(ref p) => *p != mpris::track_list::NO_TRACK.as_ref(),
                        _ => false,
                    });

                let bus = player
                    .bus()
                    .strip_prefix(mpris::BUS_NAME.as_str())
                    .and_then(|s| s.strip_prefix('.'))
                    .map_or_else(String::new, Into::into);
                let ident = player.identity().await?;
                let status = player.status();
                let position = if has_track {
                    player.position().await.ok()
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
            }
        }

        self.find_map_player(opts, Peek)
            .map_ok(Option::unwrap_or_default)
            .await
    }
}

#[zbus::interface(name = "club.bnuy.Empress.Daemon")]
impl Server {
    #[zbus(property)]
    pub async fn now_playing(&self) -> fdo::Result<PlayerStatus> {
        self.handle_method(
            self.peek_player_status(PlayerOpts::default()),
            "Error getting now-playing status",
        )
        .await
    }

    pub async fn scan(&self, #[zbus(connection)] conn: &Connection) -> fdo::Result<Vec<String>> {
        self.handle_method(
            async {
                let mut changes = Vec::new();
                self.run_scan(conn, true, Some(&mut changes)).await?;
                Ok(changes.iter().map(ToString::to_string).collect())
            },
            "Error scanning for players",
        )
        .await
    }

    pub async fn list_players(&self) -> fdo::Result<PlayerList> {
        self.handle_method(
            async {
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

    pub async fn player_status(&self, opts: PlayerOpts) -> fdo::Result<PlayerStatus> {
        self.handle_method(self.peek_player_status(opts), "Error getting player status")
            .await
    }

    pub async fn next(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            self.apply_action(opts, player::Next).map_ok(|_| ()),
            "Error skipping player forward",
        )
        .await
    }

    pub async fn prev(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            self.apply_action(opts, player::Prev).map_ok(|_| ()),
            "Error skipping player backward",
        )
        .await
    }

    pub async fn pause(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            self.apply_action(opts, player::Pause).map_ok(|_| ()),
            "Error pausing player",
        )
        .await
    }

    pub async fn play_pause(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            self.apply_action(opts, player::PlayPause).map_ok(|_| ()),
            "Error playing/pausing player",
        )
        .await
    }

    pub async fn stop(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            self.apply_action(opts, player::Stop).map_ok(|_| ()),
            "Error stopping player",
        )
        .await
    }

    pub async fn play(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            self.apply_action(opts, player::Play).map_ok(|_| ()),
            "Error playing player",
        )
        .await
    }

    pub async fn seek_relative(&self, opts: PlayerOpts, to: f64) -> fdo::Result<f64> {
        self.handle_method(
            {
                self.apply_action(opts, player::Seek(Offset::Relative(to)))
                    .map(|p| p?.context("No players available to seek"))
            },
            "Error seeking player",
        )
        .await
    }

    pub async fn seek_absolute(&self, opts: PlayerOpts, to: f64) -> fdo::Result<f64> {
        self.handle_method(
            {
                self.apply_action(opts, player::Seek(Offset::Absolute(to)))
                    .map(|p| p?.context("No players available to seek"))
            },
            "Error seeking player",
        )
        .await
    }

    pub async fn vol_relative(&self, opts: PlayerOpts, to: f64) -> fdo::Result<f64> {
        self.handle_method(
            {
                self.apply_action(opts, player::SetVolume(Offset::Relative(to)))
                    .map(|p| p?.context("No players available to get/adjust volume"))
            },
            "Error getting/adjusting player volume",
        )
        .await
    }

    pub async fn vol_absolute(&self, opts: PlayerOpts, to: f64) -> fdo::Result<f64> {
        self.handle_method(
            {
                self.apply_action(opts, player::SetVolume(Offset::Absolute(to)))
                    .map(|p| p?.context("No players available to set volume"))
            },
            "Error setting player volume",
        )
        .await
    }

    pub async fn switch_current(&self, to: &str, switch_playing: bool) -> fdo::Result<()> {
        self.handle_method(
            async {
                let to = WellKnownName::try_from(format!("{}.{to}", *mpris::BUS_NAME))
                    .map_err(|s| anyhow!("{s:?} is not a valid bus name"))?;

                let mut players = self.shared.players.write().await;
                ensure!(
                    players.contains_key(&to),
                    "No players stored with the given bus name!"
                );

                if switch_playing {
                    let mut put = vec![];

                    for player in players.iter_all() {
                        if player.playback_status().await? == PlaybackStatus::Playing
                            && player.can_pause().await?
                        {
                            put.push(player.bus().to_owned());
                        }
                    }

                    for bus in put {
                        players
                            .get_mut(&bus)
                            .unwrap()
                            .pause()
                            .await
                            .context("Error pausing an unselected player")?;
                    }

                    players
                        .get_mut(&to)
                        .unwrap()
                        .play()
                        .await
                        .context("Error playing selected player")?;
                } else {
                    players.get_mut(&to).unwrap().force_update();
                }

                Ok(())
            },
            "Error switching current player",
        )
        .await
    }
}

#[cfg(test)]
mod tests {
    use zbus::object_server::Interface;

    #[test]
    fn test_interface() {
        assert_eq!(super::Server::name(), crate::INTERFACE_ID.as_ref());
    }
}
