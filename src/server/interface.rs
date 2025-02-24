use std::{
    collections::HashMap,
    future::Future,
    panic::AssertUnwindSafe,
    sync::{Arc, LazyLock},
    time::{Duration, Instant},
};

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
    names::{BusName, InterfaceName, MemberName, OwnedInterfaceName, UniqueName, WellKnownName},
    object_server::SignalEmitter,
    zvariant::{ObjectPath, OwnedValue, Value},
    Connection, MatchRule, Message,
};

use super::{
    method_err,
    mpris::{self, player::PlaybackStatus},
    player::{self, Action},
    player_map::{PlayerChange, PlayerMut},
    Player, PlayerList, PlayerMap, PlayerOpts, PlayerStatus, PlayerStatusKind, ZResult,
};
use crate::{
    opts::Offset,
    server::{signal_matcher::SignalMatcher, PlayerMatcher},
    Result,
};

#[derive(Clone, Copy)]
enum PropInterface {
    /// org.mpris.MediaPlayer2
    Base,
    /// org.mpris.MediaPlayer2.Player
    Player,
}

enum Signal {
    Seeked { micros: i64 },
    PropChanged(PropertyChangedArgs),
}

struct PropertyChangedArgs {
    iface: PropInterface,
    changed: HashMap<String, OwnedValue>,
    invalidated: Vec<String>,
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
pub(super) struct Interface {
    shared: Arc<Shared>,
    dbus: DBusProxy<'static>,
}

struct Shared {
    players: RwLock<PlayerMap>,
}

impl Interface {
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

    fn filter_signal(msg: &Message) -> Option<Signal> {
        static IFACE_MAP: LazyLock<HashMap<&'static OwnedInterfaceName, PropInterface>> =
            LazyLock::new(|| {
                [
                    (&*mpris::INTERFACE, PropInterface::Base),
                    (&*mpris::player::INTERFACE, PropInterface::Player),
                ]
                .into_iter()
                .collect()
            });

        const PLAYER_IFACE: &str = "org.mpris.MediaPlayer2.Player";
        debug_assert_eq!(PLAYER_IFACE, mpris::player::INTERFACE.as_str());

        let hdr = msg.header();
        let iface = hdr.interface();
        let memb = hdr.member();

        match (
            iface.map(InterfaceName::as_str),
            memb.map(MemberName::as_str),
        ) {
            (Some("org.freedesktop.DBus.Properties"), Some("PropertiesChanged")) => {
                let (iface, changed, invalidated) = msg
                    .body()
                    .deserialize::<(OwnedInterfaceName, HashMap<String, OwnedValue>, Vec<String>)>()
                    .map_err(|e| {
                        debug!("Error deserializing possible PropertiesChanged event: {e}");
                    })
                    .ok()?;

                Some(Signal::PropChanged(PropertyChangedArgs {
                    iface: *IFACE_MAP.get(&iface)?,
                    changed,
                    invalidated,
                }))
            },
            (Some(PLAYER_IFACE), Some("Seeked")) => {
                let (micros,) = msg
                    .body()
                    .deserialize::<(i64,)>()
                    .map_err(|e| {
                        debug!("Error deserializing possible PropertiesChanged event: {e}");
                    })
                    .ok()?;

                Some(Signal::Seeked { micros })
            },
            _ => None,
        }
    }

    async fn spawn_background_scanner(
        self,
        ctx: SignalEmitter<'static>,
        stop_rx: mpsc::Receiver<()>,
    ) -> Result<JoinHandle<()>> {
        let mut matcher = SignalMatcher::new(self.dbus.clone(), [
            MatchRule::builder()
                .msg_type(zbus::message::Type::Signal)
                .interface("org.freedesktop.DBus.Properties")
                .unwrap()
                .member("PropertiesChanged")
                .unwrap()
                .path(&*mpris::OBJECT_PATH)
                .unwrap()
                .build(),
            MatchRule::builder()
                .msg_type(zbus::message::Type::Signal)
                .interface(&*mpris::player::INTERFACE)
                .unwrap()
                .member("Seeked")
                .unwrap()
                .path(&*mpris::OBJECT_PATH)
                .unwrap()
                .build(),
        ])
        .await
        .context("Error registering background scan matchers")?;

        let msgs = zbus::MessageStream::from(ctx.connection().clone());

        let owner_changed = self
            .dbus
            .receive_name_owner_changed()
            .await
            .context("Error creating NameOwnerChanged listener")?;

        Ok(tokio::spawn(async move {
            let shared = Arc::clone(&self.shared);
            let res = AssertUnwindSafe(self.background_scanner(ctx, stop_rx, msgs, owner_changed))
                .catch_unwind()
                .await;

            matcher
                .shutdown()
                .await
                .map_err(|e| warn!("Error unregistering background scan matchers: {e:?}"))
                .ok();

            dispose::abort_on_panic(|| res.unwrap());

            if Arc::strong_count(&shared) > 1 {
                error!("Background scanner terminated unexpectedly!");

                dispose::abort_on_panic(|| panic!("Background scanner terminated unexpectedly!"));
            }
        }))
    }

    async fn background_scanner(
        self,
        ctx: SignalEmitter<'_>,
        mut stop_rx: mpsc::Receiver<()>,
        mut msgs: zbus::MessageStream,
        mut owner_changed: zbus::fdo::NameOwnerChangedStream,
    ) {
        enum Event {
            Message(Result<Message, zbus::Error>),
            OwnerChanged(fdo::NameOwnerChanged),
        }

        while let Some(evt) = select! {
            msg = msgs.next() => msg.map(Event::Message),
            chng = owner_changed.next() => chng.map(Event::OwnerChanged),
            _ = stop_rx.recv() => None,
        } {
            let should_update = 'handle: {
                match evt {
                    Event::Message(Ok(m)) => {
                        let Some(signal) = Self::filter_signal(&m) else {
                            break 'handle false;
                        };

                        let res = match signal {
                            Signal::PropChanged(a) => self.handle_properties_changed(&m, a).await,
                            Signal::Seeked { micros } => self.handle_seeked(&m, micros).await,
                        };

                        match res {
                            Ok(r) => r,
                            Err(e) => {
                                warn!(
                                    "Error processing {} signal: {e:?}",
                                    m.header().member().map_or("???", |m| m.as_str())
                                );
                                false
                            },
                        }
                    },
                    Event::Message(Err(e)) => {
                        warn!("Received a D-Bus error: {:?}", anyhow::Error::from(e));
                        false
                    },
                    Event::OwnerChanged(chng) => {
                        const TRIES: u32 = 5;
                        const DELAY_MILLIS: u64 = 5;

                        let mut i = 0;
                        loop {
                            match self
                                .handle_name_owner_changed(ctx.connection(), &chng)
                                .await
                            {
                                Ok(r) => break r,
                                Err(e) if i == TRIES => {
                                    warn!("Error processing NameOwnerChanged signal: {e:?}");
                                    break false;
                                },
                                Err(e) => {
                                    debug!(
                                        "Error processing NameOwnerChanged signal (try {}): {e:?}",
                                        i + 1
                                    );
                                    tokio::time::sleep(Duration::from_millis(DELAY_MILLIS << i))
                                        .await;
                                },
                            }

                            i += 1;
                            assert!(i <= TRIES);
                        }
                    },
                }
            };

            if should_update {
                match self.now_playing_invalidate(&ctx).await {
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
        }
    }

    async fn resolve_signal_sender(&self, msg: &Message) -> Result<WellKnownName<'static>> {
        let header = msg.header();
        let sender = header.sender().context("Message had no sender")?;

        let players = self.shared.players.read().await;
        let sender = players
            .resolve(&sender.to_owned())
            .with_context(|| format!("Unexpected bus name {sender:?}"))?
            .to_owned();

        Ok(sender)
    }

    // Returns true if the player list needs updating
    #[inline]
    async fn handle_properties_changed(
        &self,
        msg: &Message,
        args: PropertyChangedArgs,
    ) -> Result<bool> {
        let PropertyChangedArgs {
            iface,
            changed,
            invalidated,
        } = args;

        let sender = self.resolve_signal_sender(msg).await?;
        let mut ret = false;

        let mut players = self.shared.players.write().await;
        let Some(mut player) = players.get_mut(&sender) else {
            return Ok(false);
        };

        if !changed.is_empty() {
            trace!("properties on {sender:?} changed: {changed:?}");

            for (name, val) in changed {
                match Self::handle_property_changed(&mut player, iface, &name, Some(val)) {
                    Ok(r) => ret |= r,
                    Err(e) => {
                        warn!("Error handling property change of {name:?} for {sender:?}: {e:?}");
                    },
                }
            }
        }

        if !invalidated.is_empty() {
            trace!("properties on {sender:?} invalidated: {invalidated:?}");

            for name in invalidated {
                match Self::handle_property_changed(&mut player, iface, &name, None) {
                    Ok(r) => ret |= r,
                    Err(e) => {
                        warn!(
                            "Error handling property invalidation of {name:?} for {sender:?}: \
                             {e:?}"
                        );
                    },
                }
            }
        }

        Ok(ret)
    }

    // Returns true if the player list needs updating
    fn handle_property_changed(
        player: &mut PlayerMut<'_>,
        iface: PropInterface,
        name: &str,
        val: Option<OwnedValue>,
    ) -> Result<bool> {
        match (iface, name, val) {
            (PropInterface::Base, "Identity", _) | (PropInterface::Player, "Metadata", _) => {
                Ok(true)
            },
            (PropInterface::Player, "Rate", Some(val)) => {
                player.update_rate(
                    val.downcast_ref::<f64>()
                        .context("Error downcasting playback rate")?,
                );

                Ok(false)
            },
            (PropInterface::Player, "PlaybackStatus", Some(val)) => {
                let now = Instant::now();
                let val: PlaybackStatus = val
                    .downcast_ref::<&str>()
                    .context("Error downcasting playback status")?
                    .parse()
                    .context("Error parsing playback status")?;

                Ok(player.update_status(val, now).is_some())
            },
            _ => Ok(false),
        }
    }

    // Returns true if the player list needs updating
    async fn handle_seeked(&self, msg: &Message, micros: i64) -> Result<bool> {
        let sender = self.resolve_signal_sender(msg).await?;

        trace!("position on {sender:?} changed: {micros:?}");

        let mut players = self.shared.players.write().await;
        let Some(player) = players.get_mut(&sender) else {
            return Ok(false);
        };

        player
            .force_update_position(Some(micros))
            .await
            .context("Error updating player position")?;

        Ok(true)
    }

    // Returns true if the player list needs updating
    #[inline]
    async fn handle_name_owner_changed(
        &self,
        conn: &Connection,
        chng: &fdo::NameOwnerChanged,
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

            // Depends on: metadata, bus, identity, status, position
            async fn peek(&self, player: &Player) -> Result<Option<Self::Output>> {
                let metadata = player.metadata().await?;

                let has_track = metadata
                    .get(mpris::track_list::ATTR_TRACK_ID)
                    .is_some_and(|v| match **v {
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
                let volume = player.volume().await?.unwrap_or(f64::NAN);
                let (rate, position) = if has_track {
                    player.position().await?.map(|p| (p.rate(), p.get(None)))
                } else {
                    None
                }
                .unzip();

                Ok(Some(PlayerStatus {
                    kind: if position.is_some() {
                        PlayerStatusKind::Default
                    } else {
                        PlayerStatusKind::NoPosition
                    },
                    bus,
                    ident,
                    status,
                    volume,
                    rate: rate.unwrap_or(0.0),
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
impl Interface {
    #[zbus(property(emits_changed_signal = "invalidates"))]
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

    pub async fn raise(&self, opts: PlayerOpts) -> fdo::Result<()> {
        self.handle_method(
            self.apply_action(opts, player::Raise).map_ok(|_| ()),
            "Error focusing player",
        )
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
                        if player.playback_status().await?.is_playing()
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
        assert_eq!(super::Interface::name(), crate::INTERFACE_ID.as_ref());
    }
}
