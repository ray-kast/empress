use std::{
    collections::HashMap,
    fmt::Debug,
    time::{Duration, Instant},
};

use anyhow::{anyhow, Context};
use dbus::{
    arg::{Append, AppendAll, Arg, Get, ReadAll, RefArg, Variant},
    nonblock::{stdintf::org_freedesktop_dbus::Properties, Proxy, SyncConnection},
    strings::{BusName, Member, Path},
};
use log::{log_enabled, trace, Level};

use super::{mpris, mpris::player::PlaybackStatus};
use crate::{Offset, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Player {
    pub status: PlaybackStatus,
    pub last_update: Instant,
    pub bus: BusName<'static>,
}

impl Player {
    pub async fn new(
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

    async fn call<A: Debug + AppendAll, T: Debug + ReadAll + 'static>(
        &self,
        method: &Member<'_>,
        conn: &SyncConnection,
        args: A,
    ) -> Result<T> {
        let proxy = Proxy::new(&self.bus, &*mpris::ENTRY_PATH, Duration::from_secs(2), conn);

        let args_dbg = if log_enabled!(Level::Trace) {
            Some(format!("{:?}", args))
        } else {
            None
        };

        let res = proxy
            .method_call(&*mpris::player::INTERFACE, method, args)
            .await
            .with_context(|| format!("calling {} on player {} failed", method, self.bus));

        if let Some(args_dbg) = args_dbg {
            trace!(
                "call {} on {} with {} returned {:?}",
                method,
                self.bus,
                args_dbg,
                res
            );
        }

        res
    }

    async fn get<T: Debug + for<'b> Get<'b> + 'static>(
        &self,
        prop: &Member<'_>,
        conn: &SyncConnection,
    ) -> Result<T> {
        let proxy = Proxy::new(&self.bus, &*mpris::ENTRY_PATH, Duration::from_secs(2), conn);

        let res = proxy
            .get(&*mpris::player::INTERFACE, prop)
            .await
            .with_context(|| format!("getting {} on player {} failed", prop, self.bus));

        trace!("get {} on {} returned {:?}", prop, self.bus, res);

        res
    }

    async fn set<T: Debug + Arg + Append>(
        &self,
        prop: &Member<'_>,
        conn: &SyncConnection,
        value: T,
    ) -> Result<()> {
        let proxy = Proxy::new(&self.bus, &*mpris::ENTRY_PATH, Duration::from_secs(2), conn);

        let value_dbg = if log_enabled!(Level::Trace) {
            Some(format!("{:?}", value))
        } else {
            None
        };

        let res = proxy
            .set(&*mpris::player::INTERFACE, prop, value)
            .await
            .with_context(|| format!("setting {} on player {} failed", prop, self.bus));

        if let Some(value_dbg) = value_dbg {
            trace!(
                "set {} on {} to {} returned {:?}",
                prop,
                self.bus,
                value_dbg,
                res
            );
        }

        res
    }

    pub async fn next(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::NEXT, conn, ()).await?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn previous(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::PREVIOUS, conn, ()).await?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn pause(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::PAUSE, conn, ()).await?;

        Ok(Self {
            status: PlaybackStatus::Paused,
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn stop(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::STOP, conn, ()).await?;

        Ok(Self {
            status: PlaybackStatus::Stopped,
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn play(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::PLAY, conn, ()).await?;

        Ok(Self {
            status: PlaybackStatus::Playing,
            last_update: Instant::now(),
            ..self
        })
    }

    #[allow(clippy::cast_possible_truncation)]
    pub async fn set_position(
        self,
        conn: &SyncConnection,
        id: Path<'_>,
        secs: f64,
    ) -> Result<Self> {
        self.call(
            &*mpris::player::SET_POSITION,
            conn,
            (id, (secs * 1e6).round() as i64),
        )
        .await?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn playback_status(&self, conn: &SyncConnection) -> Result<PlaybackStatus> {
        self.get(&*mpris::player::PLAYBACK_STATUS, conn)
            .await
            .and_then(|s: String| s.parse())
    }

    pub async fn metadata(
        &self,
        conn: &SyncConnection,
    ) -> Result<HashMap<String, Variant<Box<dyn RefArg>>>> {
        self.get(&*mpris::player::METADATA, conn).await
    }

    pub async fn volume(&self, conn: &SyncConnection) -> Result<f64> {
        self.get(&*mpris::player::VOLUME, conn).await
    }

    pub async fn set_volume(&self, conn: &SyncConnection, vol: f64) -> Result<()> {
        self.set(&*mpris::player::VOLUME, conn, vol).await
    }

    #[allow(clippy::cast_precision_loss)]
    pub async fn position(&self, conn: &SyncConnection) -> Result<f64> {
        self.get(&*mpris::player::POSITION, conn)
            .await
            .map(|u: i64| u as f64 / 1e6)
    }

    pub async fn can_go_next(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_GO_NEXT, conn).await
    }

    pub async fn can_go_previous(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_GO_PREVIOUS, conn).await
    }

    pub async fn can_play(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_PLAY, conn).await
    }

    pub async fn can_pause(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_PAUSE, conn).await
    }

    pub async fn can_seek(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_SEEK, conn).await
    }

    pub async fn can_control(&self, conn: &SyncConnection) -> Result<bool> {
        self.get(&*mpris::player::CAN_CONTROL, conn).await
    }

    pub async fn try_next(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(if self.can_go_next(conn).await? {
            Some(self.next(conn).await?)
        } else {
            None
        })
    }

    pub async fn try_previous(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(if self.can_go_previous(conn).await? {
            Some(self.previous(conn).await?)
        } else {
            None
        })
    }

    pub async fn try_pause(self, conn: &SyncConnection) -> Result<Option<Self>> {
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

    pub async fn try_play_pause(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(match self.playback_status(conn).await? {
            PlaybackStatus::Playing if self.can_pause(conn).await? => Some(self.pause(conn).await?),
            PlaybackStatus::Paused if self.can_play(conn).await? => Some(self.play(conn).await?),
            _ => None,
        })
    }

    pub async fn try_stop(self, conn: &SyncConnection) -> Result<Option<Self>> {
        Ok(match self.playback_status(conn).await? {
            PlaybackStatus::Playing | PlaybackStatus::Paused if self.can_control(conn).await? => {
                Some(self.stop(conn).await?)
            },
            _ => None,
        })
    }

    pub async fn try_play(self, conn: &SyncConnection) -> Result<Option<Self>> {
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

    pub async fn try_seek(self, conn: &SyncConnection, to: Offset) -> Result<Option<(Self, f64)>> {
        Ok(if self.can_seek(conn).await? {
            let meta = self.metadata(conn).await?;

            let pos = match to {
                Offset::Relative(p) => self.position(conn).await? + p,
                Offset::Absolute(p) => p,
            };

            Some((
                self.set_position(
                    conn,
                    Path::new(
                        meta.get(mpris::track_list::ATTR_TRACKID)
                            .ok_or_else(|| anyhow!("missing track ID in metadata"))?
                            .as_str()
                            .ok_or_else(|| anyhow!("track ID wasn't a string"))?,
                    )
                    .map_err(|s| anyhow!("track ID {:?} was not valid", s))?,
                    pos,
                )
                .await?,
                pos,
            ))
        } else {
            None
        })
    }

    pub async fn try_set_volume(
        self,
        conn: &SyncConnection,
        vol: Offset,
    ) -> Result<Option<(Self, f64)>> {
        let (vol, set) = match vol {
            Offset::Relative(v) => {
                let prev = self.volume(conn).await?;
                let next = prev + v;

                if (next - prev).abs() > 1e-5 {
                    (next, true)
                } else {
                    (prev, false)
                }
            },
            Offset::Absolute(v) => (v, true),
        };

        if !vol.is_finite() {
            return Err(anyhow!("Invalid volume {:?}", vol));
        }

        Ok(if set {
            if self.can_control(conn).await? {
                // Safety check
                let vol = vol.max(0.0).min(1.0);

                self.set_volume(conn, vol).await?;

                Some((self, vol))
            } else {
                None
            }
        } else {
            Some((self, vol))
        })
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
