use std::{
    collections::{BTreeSet, HashMap, HashSet},
    future::Future,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{Context, Error};
use dbus::{
    arg::RefArg,
    channel::MatchingReceiver,
    message::MatchRule,
    nonblock::{
        stdintf::org_freedesktop_dbus::{Properties, PropertiesPropertiesChanged},
        MsgMatch, Proxy, SyncConnection,
    },
    strings::{BusName, Member},
    MethodErr,
};
use dbus_crossroads::{Crossroads, IfaceBuilder};
use dbus_tokio::connection;
use mpris::player::PlaybackStatus;
use tokio::{
    select,
    signal::{unix, unix::SignalKind},
    sync::{mpsc, oneshot, RwLock},
};

mod mpris {
    use dbus::{
        strings::{BusName, Interface, Member},
        Path,
    };
    use lazy_static::lazy_static;

    lazy_static! {
        pub static ref BUS_NAME: BusName<'static> = "org.mpris.MediaPlayer2".into();
        pub static ref ENTRY_PATH: Path<'static> = "/org/mpris/MediaPlayer2".into();
    }

    pub mod player {
        use super::*;

        lazy_static! {
            pub static ref INTERFACE: Interface<'static> = "org.mpris.MediaPlayer2.Player".into();
            pub static ref NEXT: Member<'static> = "Next".into();
            pub static ref PREVIOUS: Member<'static> = "Previous".into();
            pub static ref PAUSE: Member<'static> = "Pause".into();
            pub static ref STOP: Member<'static> = "Stop".into();
            pub static ref PLAY: Member<'static> = "Play".into();
            pub static ref PLAYBACK_STATUS: Member<'static> = "PlaybackStatus".into();
            pub static ref CAN_GO_NEXT: Member<'static> = "CanGoNext".into();
            pub static ref CAN_GO_PREVIOUS: Member<'static> = "CanGoPrevious".into();
            pub static ref CAN_PLAY: Member<'static> = "CanPlay".into();
            pub static ref CAN_PAUSE: Member<'static> = "CanPause".into();
            pub static ref CAN_CONTROL: Member<'static> = "CanControl".into();
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        pub enum PlaybackStatus {
            Playing,
            Paused,
            Stopped,
        }

        impl std::fmt::Display for PlaybackStatus {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(match self {
                    Self::Playing => "Playing",
                    Self::Paused => "Paused",
                    Self::Stopped => "Stopped",
                })
            }
        }

        impl std::str::FromStr for PlaybackStatus {
            type Err = anyhow::Error;

            fn from_str(s: &str) -> crate::Result<Self> {
                Ok(match s {
                    "Playing" => Self::Playing,
                    "Paused" => Self::Paused,
                    "Stopped" => Self::Stopped,
                    s => return Err(anyhow::anyhow!("unexpected playback status {:?}", s)),
                })
            }
        }
    }
}

use crate::{MethodId, Result, INTERFACE_NAME, METHOD_IDS, SERVER_NAME, SERVER_PATH};

fn method_err(e: impl Into<Error>, msg: impl std::fmt::Display) -> MethodErr {
    eprintln!("ERROR: {:?}", e.into().context(msg.to_string()));
    MethodErr::failed(&msg)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Player {
    status: PlaybackStatus,
    last_update: Instant,
    bus: BusName<'static>,
}

impl Player {
    async fn new(
        last_update: Instant,
        name: impl Into<BusName<'static>>,
        conn: &SyncConnection,
    ) -> Result<Self> {
        let mut ret = Self {
            status: PlaybackStatus::Stopped,
            last_update,
            bus: name.into(),
        };

        ret.status = ret.playback_status(conn).await?;

        Ok(ret)
    }

    async fn call<T: dbus::arg::ReadAll + 'static>(
        &self,
        method: &Member<'_>,
        conn: &SyncConnection,
    ) -> Result<T> {
        let proxy = Proxy::new(&self.bus, &*mpris::ENTRY_PATH, Duration::from_secs(2), conn);

        proxy
            .method_call(&*mpris::player::INTERFACE, method, ())
            .await
            .with_context(|| format!("calling {} on player {} failed", method, self.bus))
    }

    async fn get<T: for<'b> dbus::arg::Get<'b> + 'static>(
        &self,
        prop: &Member<'_>,
        conn: &SyncConnection,
    ) -> Result<T> {
        let proxy = Proxy::new(&self.bus, &*mpris::ENTRY_PATH, Duration::from_secs(2), conn);

        proxy
            .get(&*mpris::player::INTERFACE, prop)
            .await
            .with_context(|| format!("getting {} on player {} failed", prop, self.bus))
    }

    async fn next(self, conn: &SyncConnection) -> Result<Self> {
        let () = self.call(&*mpris::player::NEXT, conn).await?;

        Ok(Self {
            status: self.status,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    async fn previous(self, conn: &SyncConnection) -> Result<Self> {
        let () = self.call(&*mpris::player::PREVIOUS, conn).await?;

        Ok(Self {
            status: self.status,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    async fn pause(self, conn: &SyncConnection) -> Result<Self> {
        let () = self.call(&*mpris::player::PAUSE, conn).await?;

        Ok(Self {
            status: PlaybackStatus::Paused,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    async fn stop(self, conn: &SyncConnection) -> Result<Self> {
        let () = self.call(&*mpris::player::STOP, conn).await?;

        Ok(Self {
            status: PlaybackStatus::Stopped,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    async fn play(self, conn: &SyncConnection) -> Result<Self> {
        let () = self.call(&*mpris::player::PLAY, conn).await?;

        Ok(Self {
            status: PlaybackStatus::Playing,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    async fn playback_status(&self, conn: &SyncConnection) -> Result<PlaybackStatus> {
        self.get(&*mpris::player::PLAYBACK_STATUS, conn)
            .await
            .and_then(|s: String| s.parse())
    }

    async fn can_go_next(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_GO_NEXT, conn).await
    }

    async fn can_go_previous(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_GO_PREVIOUS, conn).await
    }

    async fn can_play(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_PLAY, conn).await
    }

    async fn can_pause(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_PAUSE, conn).await
    }

    async fn can_control(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_CONTROL, conn).await
    }

    async fn try_next(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(if self.can_go_next(conn).await? {
            Some(self.next(conn).await?)
        } else {
            None
        })
    }

    async fn try_previous(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(if self.can_go_previous(conn).await? {
            Some(self.previous(conn).await?)
        } else {
            None
        })
    }

    async fn try_pause(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(
            if self.playback_status(conn).await? == PlaybackStatus::Playing
                && self.can_pause(conn).await?
            {
                Some(self.pause(conn).await?)
            } else {
                None
            },
        )
    }

    async fn try_play_pause(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(match self.playback_status(conn).await? {
            PlaybackStatus::Playing if self.can_pause(conn).await? => Some(self.pause(conn).await?),
            PlaybackStatus::Paused if self.can_play(conn).await? => Some(self.play(conn).await?),
            _ => None,
        })
    }

    async fn try_stop(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(match self.playback_status(conn).await? {
            PlaybackStatus::Playing | PlaybackStatus::Paused if self.can_control(conn).await? => {
                Some(self.stop(conn).await?)
            },
            _ => None,
        })
    }

    async fn try_play(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(
            if self.playback_status(conn).await? != PlaybackStatus::Playing
                && self.can_play(conn).await?
            {
                Some(self.play(conn).await?)
            } else {
                None
            },
        )
    }
}

impl PartialOrd for Player {
    fn partial_cmp(&self, rhs: &Player) -> Option<std::cmp::Ordering> { Some(self.cmp(rhs)) }
}

impl Ord for Player {
    fn cmp(&self, rhs: &Player) -> std::cmp::Ordering {
        self.status
            .cmp(&rhs.status)
            .then_with(|| rhs.last_update.cmp(&self.last_update))
            .then_with(|| self.bus.cmp(&rhs.bus))
    }
}

#[derive(Debug)]
struct PlayerMap(
    HashMap<BusName<'static>, (PlaybackStatus, Instant)>,
    BTreeSet<Player>,
);

impl PlayerMap {
    fn new() -> Self { Self(HashMap::new(), BTreeSet::new()) }

    fn iter(&self) -> impl Iterator<Item = &Player> { self.1.iter() }

    async fn inform<P: Fn(&BusName<'static>) -> PR, PR: Future<Output = Result<Player>>>(
        &mut self,
        names: HashSet<BusName<'static>>,
        player: P,
    ) -> Result<()> {
        use std::collections::hash_map::Entry;

        let key_set = self.0.keys().cloned().collect();

        for name in names.intersection(&key_set) {
            let player = player(name).await?;
            let (status, last_update) = self.0.get(name).unwrap();

            if player.status != *status || player.last_update < *last_update {
                self.put(player);
            }
        }

        for name in names.symmetric_difference(&self.0.keys().cloned().collect()) {
            match self.0.entry(name.clone()) {
                Entry::Vacant(v) => {
                    let player = player(v.key()).await?;

                    v.insert((player.status, player.last_update));
                    self.1.insert(player);
                },
                Entry::Occupied(o) => {
                    let (bus, (status, last_update)) = o.remove_entry();
                    assert!(self.1.remove(&Player {
                        status,
                        last_update,
                        bus
                    }));
                },
            }
        }

        Ok(())
    }

    fn put(&mut self, player: Player) -> bool {
        use std::collections::hash_map::Entry;

        match self.0.entry(player.bus.clone()) {
            Entry::Vacant(v) => {
                v.insert((player.status, player.last_update));
                self.1.insert(player);
                true
            },
            Entry::Occupied(o) => {
                let (status, last_update) = *o.get();
                assert!(self.1.remove(&Player {
                    status,
                    last_update,
                    bus: player.bus.clone()
                }));
                *o.into_mut() = (player.status, player.last_update);
                self.1.insert(player);
                false
            },
        }
    }

    fn drop(&mut self, bus: BusName<'static>) -> bool {
        if let Some((status, last_update)) = self.0.remove(&bus) {
            self.1.remove(&Player {
                status,
                last_update,
                bus,
            });
            true
        } else {
            false
        }
    }
}

struct Server {
    conn: Arc<SyncConnection>,
    players: RwLock<PlayerMap>,
    prop_changed: MsgMatch,
}

impl Server {
    async fn new(conn: Arc<SyncConnection>) -> Result<Arc<Server>> {
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
                if changed.interface_name != mpris::player::INTERFACE.as_str().unwrap() {
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
            while let Some(()) = scan_rx.recv().await {
                eprintln!("Rescanning...");
                self_1.scan().await.unwrap();

                tokio::time::sleep(Duration::from_millis(200)).await;
            }
        });

        ret.scan().await?;

        Ok(ret)
    }

    async fn scan(&self) -> Result {
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

        self.players
            .write()
            .await
            .inform(
                names
                    .into_iter()
                    .filter(|n| n.starts_with(&*mpris::BUS_NAME as &str))
                    .map(|n| n.into())
                    .collect(),
                |n| Player::new(now, n.clone(), &*self.conn),
            )
            .await?;

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

    async fn handle(&self, id: MethodId) -> Result<(), MethodErr> {
        eprintln!("handling MethodId::{:?}", id);

        self.scan()
            .await
            .map_err(|e| method_err(e, "failed to scan for players"))?;

        match id {
            MethodId::Next => {
                self.process_player(|p| p.try_next(&*self.conn))
                    .await
                    .map_err(|e| method_err(e, "attempting to go next on a player failed"))?;
            },
            MethodId::Previous => {
                self.process_player(|p| p.try_previous(&*self.conn))
                    .await
                    .map_err(|e| method_err(e, "attempting to go previous on a player failed"))?;
            },
            MethodId::Pause => {
                self.process_player(|p| p.try_pause(&*self.conn))
                    .await
                    .map_err(|e| method_err(e, "attempting to pause a player failed"))?;
            },
            MethodId::PlayPause => {
                self.process_player(|p| p.try_play_pause(&*self.conn))
                    .await
                    .map_err(|e| method_err(e, "attempting to play/pause a player failed"))?;
            },
            MethodId::Stop => {
                self.process_player(|p| p.try_stop(&*self.conn))
                    .await
                    .map_err(|e| method_err(e, "attempting to stop a player failed"))?;
            },
            MethodId::Play => {
                self.process_player(|p| p.try_play(&*self.conn))
                    .await
                    .map_err(|e| method_err(e, "attempting to play a player failed"))?;
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

pub async fn run() -> Result {
    let (res, conn) = connection::new_session_sync().context("failed to connect to D-Bus")?;
    let (close_tx, close_rx) = oneshot::channel();

    tokio::spawn(async {
        close_tx
            .send(Error::from(res.await).context("D-Bus disconnected"))
            .ok();
    });

    conn.request_name(&*SERVER_NAME, false, true, false)
        .await
        .context("failed to request server name")?;

    let mut cr = Crossroads::new();

    cr.set_async_support(Some((
        conn.clone(),
        Box::new(|x| {
            tokio::spawn(x);
        }),
    )));

    let tok = cr.register(&*INTERFACE_NAME, |b: &mut IfaceBuilder<Arc<Server>>| {
        for id in METHOD_IDS.iter().copied() {
            b.method_with_cr_async(id.to_string(), (), (), move |mut ctx, cr, ()| {
                let serv = cr.data_mut::<Arc<Server>>(ctx.path()).cloned();

                async move {
                    let serv = match serv {
                        Some(s) => s,
                        None => return ctx.reply(Err(MethodErr::no_path(ctx.path()))),
                    };

                    let res = serv.handle(id).await;

                    ctx.reply(res)
                }
            });
        }
    });

    cr.insert(
        &*SERVER_PATH,
        &[tok],
        Server::new(conn.clone())
            .await
            .context("failed to initialize server")?,
    );

    conn.start_receive(
        MatchRule::new_method_call(),
        Box::new(move |msg, conn| {
            let msg_dbg = format!("{:?}", msg);

            match cr.handle_message(msg, conn) {
                Ok(()) => (),
                Err(()) => eprintln!("WARNING: failed to handle message {}", msg_dbg),
            };
            true
        }),
    );

    let mut hup = unix::signal(SignalKind::hangup()).context("failed to hook SIGHUP")?;
    let mut int = unix::signal(SignalKind::interrupt()).context("failed to hook SIGINT")?;
    let mut quit = unix::signal(SignalKind::quit()).context("failed to hook SIGQUIT")?;
    let mut term = unix::signal(SignalKind::terminate()).context("failed to hook SIGTERM")?;

    select!(
        Some(()) = hup.recv() => Ok(()),
        Some(()) = int.recv() => Ok(()),
        Some(()) = quit.recv() => Ok(()),
        Some(()) = term.recv() => Ok(()),
        res = close_rx => Err(
            res.context("lost D-Bus connection resource").map_or_else(|e| e, |e| e)
        ),
    )?;

    eprintln!("Shutting down...");

    Ok(())
}
