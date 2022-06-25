use std::{
    collections::HashMap,
    sync::Arc,
    time::{Duration, Instant},
};

use futures::{prelude::*, Future};
use log::{debug, trace, warn};
use tokio::sync::RwLock;
use zbus::{
    fdo::DBusProxy,
    names::OwnedBusName,
    zvariant::{OwnedObjectPath, Value},
    Connection,
};

use super::player_map::PlayerMap;
use crate::{
    interface::{
        metadata, mpris, Error, NowPlayingResponse, PlaybackStatus, PlayerSearchOpts, Result,
    },
    server::player::Player,
    Offset,
};

struct ConnectionState {}

pub struct Handler {
    players: RwLock<PlayerMap>,
}

impl Handler {
    pub fn new() -> Arc<Self> {
        let players = RwLock::new(PlayerMap::new());

        // TODO
        // let mut mr = MatchRule::new_signal("org.freedesktop.DBus.Properties",
        // "PropertiesChanged"); mr.path = Some(mpris::ENTRY_PATH.clone());
        // mr.path_is_namespace = false;

        // let (scan_tx, mut scan_rx) = mpsc::channel(1);

        // let prop_changed = conn
        //     .add_match(mr)
        //     .await
        //     .context("failed to listen for property changes")?
        //     .cb(move |_, changed: PropertiesPropertiesChanged| {
        //         if changed.interface_name.as_str() != &**mpris::player::INTERFACE {
        //             debug!(
        //                 "Ignoring PropertiesChanged for interface {:?}",
        //                 changed.interface_name
        //             );

        //             return true;
        //         }

        //         scan_tx.try_send(()).ok();
        //         true
        //     });

        let ret = Arc::new(Self { players });

        // MORE TODO
        // let self_1 = ret.clone();
        // tokio::spawn(async move {
        //     'main: while let Some(()) = scan_rx.recv().await {
        //         debug!("Pending background scan...");

        //         loop {
        //             match select!(
        //                 opt = scan_rx.recv() => opt.map(|()| false),
        //                 () = tokio::time::sleep(Duration::from_millis(200)) =>
        // Some(true),             ) {
        //                 Some(true) => break,
        //                 Some(false) => (),
        //                 None => break 'main,
        //             }
        //         }

        //         match self_1.scan(true).await {
        //             Ok(()) => (),
        //             Err(e) => warn!("Background scan failed: {:?}", e),
        //         }
        //     }
        // });

        // tokio::spawn(async {
        //     ret.scan(false)
        //         .await
        //         .map_err(|e| error!("Initial scan failed: {}", e));
        // });

        ret
    }

    async fn scan(&self, conn: &Connection, force: bool) -> Result {
        if force {
            debug!("Running full scan...");
        } else {
            trace!("Running quick scan...");
        }

        let now = Instant::now();
        let proxy = DBusProxy::new(conn).await?;

        PlayerMap::inform(
            &self.players,
            force,
            proxy
                .list_names()
                .await?
                .into_iter()
                .filter(|n| n.starts_with(&*mpris::NAME_PREFIX))
                .map(Into::into)
                .collect(),
            |n| Player::new(now, n.into(), conn),
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

        if let Some((next, ret)) = patch.or_else(|e| {
            e.map_or(Ok(None), |()| {
                Err(Error::Failed("All players failed to process".into()))
            })
        })? {
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

        default.or_else(|e| {
            e.map_or(Ok(None), |()| {
                Err(Error::Failed("All players failed to peek".into()))
            })
        })
    }

    async fn process_player<F: Fn(Player) -> FR, FR: Future<Output = Result<Option<Player>>>>(
        &self,
        f: F,
    ) -> Result<bool> {
        self.process_player_with(|p| f(p).map_ok(|p| p.map(|p| (p, ()))))
            .await
            .map(|o| matches!(o, Some(())))
    }

    pub async fn handle<'a, T, U, F: Future<Output = Result<U>>>(
        &'a self,
        f: impl FnOnce(&'a Self, &'a Connection, T) -> F + 'a,
        conn: &'a Connection,
        arg: T,
    ) -> Result<U> {
        self.scan(conn, false).await;

        f(self, conn, arg).await
    }

    pub async fn list_players(
        this: &Self,
        conn: &Connection,
        (): (),
    ) -> Result<Vec<(String, PlaybackStatus)>> {
        Ok(this
            .players
            .read()
            .await
            .iter_all()
            .filter_map(|p| {
                p.bus
                    .0
                    .strip_prefix(&*mpris::NAME_PREFIX)
                    .and_then(|s| s.strip_prefix('.'))
                    .map(|s| (s.into(), p.status))
            })
            .collect())
    }

    pub async fn now_playing(
        this: &Self,
        conn: &Connection,
        search: PlayerSearchOpts,
    ) -> Result<NowPlayingResponse> {
        this.peek_player_with(|p| async move {
            let meta = p.metadata(conn).await?;

            let has_track = meta
                .get(mpris::track_list::ATTR_TRACKID)
                .and_then(|v| v.downcast_ref())
                .map_or(false, |p| p == *mpris::track_list::NO_TRACK);

            let bus = p
                .bus
                .0
                .strip_prefix(&*mpris::NAME_PREFIX)
                .and_then(|s| s.strip_prefix('.'))
                .map_or_else(String::new, Into::into);
            let ident = p.identity(conn).await?;

            // Properties that should go into the map regardless of if we have a track
            let extra_props: [(String, Value); 2] = [
                (metadata::PLAYER_BUS.into(), bus.into()),
                (metadata::PLAYER_IDENTITY.into(), ident.into()),
            ];

            Ok(Some(if has_track {
                let mut meta: HashMap<_, _> = meta.into_iter().chain(extra_props).collect();

                let pos = p.position(conn).await.ok();

                if let Some(pos) = pos {
                    meta.insert(metadata::POSITION.into(), pos.into());
                }

                (meta, p.status)
            } else {
                (extra_props.into_iter().collect(), p.status)
            }))
        })
        .map_ok(|ok| ok.unwrap_or_else(|| (HashMap::new(), PlaybackStatus::Stopped)))
        .await
    }

    pub async fn next(this: &Self, conn: &Connection, search: PlayerSearchOpts) -> Result {
        this.process_player(|p| p.try_next(conn))
            .map_ok(|_| ())
            .await
    }

    pub async fn previous(this: &Self, conn: &Connection, search: PlayerSearchOpts) -> Result {
        this.process_player(|p| p.try_previous(conn))
            .map_ok(|_| ())
            .await
    }

    pub async fn pause(this: &Self, conn: &Connection, search: PlayerSearchOpts) -> Result {
        this.process_player(|p| p.try_pause(conn))
            .map_ok(|_| ())
            .await
    }

    pub async fn play_pause(this: &Self, conn: &Connection, search: PlayerSearchOpts) -> Result {
        this.process_player(|p| p.try_play_pause(conn))
            .map_ok(|_| ())
            .await
    }

    pub async fn stop(this: &Self, conn: &Connection, search: PlayerSearchOpts) -> Result {
        this.process_player(|p| p.try_stop(conn))
            .map_ok(|_| ())
            .await
    }

    pub async fn play(this: &Self, conn: &Connection, search: PlayerSearchOpts) -> Result {
        this.process_player(|p| p.try_play(conn))
            .map_ok(|_| ())
            .await
    }

    pub async fn seek_relative(
        this: &Self,
        conn: &Connection,
        (search, to): (PlayerSearchOpts, f64),
    ) -> Result<f64> {
        this.process_player_with(|p| p.try_seek(conn, Offset::Relative(to)))
            .map(|p| {
                p?.ok_or_else(|| Error::Failed("No players available to seek".into()))
                    .map(|p| (p,))
            })
            .await
    }

    pub async fn seek_absolute(
        this: &Self,
        conn: &Connection,
        (search, to): (PlayerSearchOpts, f64),
    ) -> Result<f64> {
        this.process_player_with(|p| p.try_seek(conn, Offset::Absolute(to)))
            .map(|p| {
                p?.ok_or_else(|| Error::Failed("No players available to seek".into()))
                    .map(|p| (p,))
            })
            .await
    }

    pub async fn vol_relative(
        this: &Self,
        conn: &Connection,
        (search, by): (PlayerSearchOpts, f64),
    ) -> Result<f64> {
        this.process_player_with(|p| p.try_set_volume(conn, Offset::Relative(by)))
            .map(|p| {
                p?.ok_or_else(|| Error::Failed("No players available to get/adjust volume".into()))
                    .map(|p| (p,))
            })
            .await
    }

    pub async fn vol_absolute(
        this: &Self,
        conn: &Connection,
        (search, to): (PlayerSearchOpts, f64),
    ) -> Result<f64> {
        this.process_player_with(|p| p.try_set_volume(conn, Offset::Absolute(to)))
            .map(|p| {
                p?.ok_or_else(|| Error::Failed("No players available to set volume".into()))
                    .map(|p| (p,))
            })
            .await
    }

    pub async fn switch_current<D: std::fmt::Display>(
        this: &Self,
        conn: &Connection,
        (to, switch_playing): (D, bool),
    ) -> Result<()> {
        let bus = OwnedBusName::try_from(format!("{}.{}", *mpris::BUS_NAME, to))
            .map_err(|s| Error::InvalidArgs(format!("{:?} is not a valid bus name", todo!())))?;

        let curr = match this.players.write().await.remove(&bus) {
            Some(c) => c,
            None => {
                return Err(Error::InvalidArgs(
                    "No players stored with the given bus name".into(),
                ));
            },
        };

        let mut curr = if switch_playing {
            let mut put = vec![];

            for player in this.players.read().await.iter_all() {
                if player.playback_status(conn).await? == PlaybackStatus::Playing
                    && player.can_pause(conn).await?
                {
                    put.push(player.bus.clone());
                }
            }

            let mut players = this.players.write().await;

            for bus in put {
                let ply = players.remove(&bus.0).unwrap();

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
        this.players.write().await.put(curr);

        Ok(())
    }
}
