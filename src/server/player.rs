use std::{collections::HashMap, fmt::Debug, time::Instant};

use anyhow::{anyhow, Context};
use log::{log_enabled, trace, Level};
use serde::{de::DeserializeOwned, Serialize};
use zbus::{
    names::{InterfaceName, MemberName, OwnedBusName},
    zvariant::{DynamicType, ObjectPath, OwnedValue, Type, Value},
    Connection, Proxy,
};

use super::{mpris, mpris::player::PlaybackStatus};
use crate::{Offset, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Player {
    pub status: PlaybackStatus,
    pub last_update: Instant,
    pub bus: OwnedBusName,
}

impl Player {
    pub async fn new(
        last_update: Instant,
        name: impl Into<OwnedBusName>,
        conn: &Connection,
    ) -> Result<Self> {
        let mut ret = Self {
            status: PlaybackStatus::Stopped,
            last_update,
            bus: name.into(),
        };

        ret.status = ret.playback_status(conn).await?;

        Ok(ret)
    }

    async fn call<
        A: DynamicType + Serialize + Debug,
        T: Type + DeserializeOwned + Debug + 'static,
    >(
        &self,
        interface: &InterfaceName<'_>,
        method: &MemberName<'_>,
        conn: &Connection,
        args: &A,
    ) -> Result<T> {
        // TODO: declare timeout
        let proxy = Proxy::new(conn, &self.bus, &*mpris::ENTRY_PATH, interface)
            .await
            .context("failed to create player proxy")?;

        let args_dbg = if log_enabled!(Level::Trace) {
            Some(format!("{:?}", args))
        } else {
            None
        };

        let res = proxy.call(method, args).await.with_context(|| {
            format!(
                "calling {}::{} on player {} failed",
                interface, method, self.bus
            )
        });

        if let Some(args_dbg) = args_dbg {
            trace!(
                "call {}::{} on {} with {} returned {:?}",
                interface,
                method,
                self.bus,
                args_dbg,
                res
            );
        }

        res
    }

    async fn get<T: TryFrom<OwnedValue> + Debug + 'static>(
        &self,
        interface: &InterfaceName<'_>,
        prop: &MemberName<'_>,
        conn: &Connection,
    ) -> Result<T>
    where
        T::Error: Into<zbus::Error>,
    {
        // TODO: declare timeout
        let proxy = Proxy::new(conn, &self.bus, &*mpris::ENTRY_PATH, interface)
            .await
            .context("failed to create player proxy")?;

        let res = proxy.get_property(prop).await.with_context(|| {
            format!(
                "getting {}::{} on player {} failed",
                interface, prop, self.bus
            )
        });

        trace!(
            "get {}::{} on {} returned {:?}",
            interface,
            prop,
            self.bus,
            res
        );

        res
    }

    async fn set<T: for<'t> Into<Value<'t>> + Debug>(
        &self,
        interface: &InterfaceName<'_>,
        prop: &MemberName<'_>,
        conn: &Connection,
        value: T,
    ) -> Result<()> {
        // TODO: declare timeout
        let proxy = Proxy::new(
            conn,
            &self.bus,
            &*mpris::ENTRY_PATH,
            &*mpris::player::INTERFACE,
        )
        .await
        .context("failed to create player proxy")?;

        let value_dbg = if log_enabled!(Level::Trace) {
            Some(format!("{:?}", value))
        } else {
            None
        };

        let res = proxy.set_property(prop, value).await.with_context(|| {
            format!(
                "setting {}::{} on player {} failed",
                interface, prop, self.bus
            )
        });

        if let Some(value_dbg) = value_dbg {
            trace!(
                "set {}::{} on {} to {} returned {:?}",
                interface,
                prop,
                self.bus,
                value_dbg,
                res
            );
        }

        res
    }

    //////// Methods under MediaPlayer2.Player ////////

    pub async fn next(self, conn: &Connection) -> Result<Self> {
        self.call(&mpris::player::INTERFACE, &mpris::player::NEXT, conn, &())
            .await?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn previous(self, conn: &Connection) -> Result<Self> {
        self.call(
            &mpris::player::INTERFACE,
            &mpris::player::PREVIOUS,
            conn,
            &(),
        )
        .await?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn pause(self, conn: &Connection) -> Result<Self> {
        self.call(&mpris::player::INTERFACE, &mpris::player::PAUSE, conn, &())
            .await?;

        Ok(Self {
            status: PlaybackStatus::Paused,
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn stop(self, conn: &Connection) -> Result<Self> {
        self.call(&mpris::player::INTERFACE, &mpris::player::STOP, conn, &())
            .await?;

        Ok(Self {
            status: PlaybackStatus::Stopped,
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn play(self, conn: &Connection) -> Result<Self> {
        self.call(&mpris::player::INTERFACE, &mpris::player::PLAY, conn, &())
            .await?;

        Ok(Self {
            status: PlaybackStatus::Playing,
            last_update: Instant::now(),
            ..self
        })
    }

    #[allow(clippy::cast_possible_truncation)]
    pub async fn set_position(
        self,
        conn: &Connection,
        id: ObjectPath<'_>,
        secs: i64,
    ) -> Result<Self> {
        self.call(
            &mpris::player::INTERFACE,
            &mpris::player::SET_POSITION,
            conn,
            &(id, secs),
        )
        .await?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    //////// Properties under MediaPlayer2 ////////

    pub async fn identity(&self, conn: &Connection) -> Result<String> {
        self.get(&mpris::root::INTERFACE, &mpris::root::IDENTITY, conn)
            .await
    }

    //////// Properties under MediaPlayer2.Player ////////

    pub async fn playback_status(&self, conn: &Connection) -> Result<PlaybackStatus> {
        self.get(
            &mpris::player::INTERFACE,
            &mpris::player::PLAYBACK_STATUS,
            conn,
        )
        .await
        .and_then(|s: String| s.parse().context("Invalid playback status"))
    }

    pub async fn metadata(&self, conn: &Connection) -> Result<HashMap<String, Value<'static>>> {
        self.get(&mpris::player::INTERFACE, &mpris::player::METADATA, conn)
            .await
    }

    pub async fn volume(&self, conn: &Connection) -> Result<f64> {
        self.get(&mpris::player::INTERFACE, &mpris::player::VOLUME, conn)
            .await
    }

    pub async fn set_volume(&self, conn: &Connection, vol: f64) -> Result<()> {
        self.set(&mpris::player::INTERFACE, &mpris::player::VOLUME, conn, vol)
            .await
    }

    #[allow(clippy::cast_precision_loss)]
    pub async fn position(&self, conn: &Connection) -> Result<i64> {
        self.get(&mpris::player::INTERFACE, &mpris::player::POSITION, conn)
            .await
    }

    pub async fn can_go_next(&self, conn: &Connection) -> Result<bool> {
        self.get(&mpris::player::INTERFACE, &mpris::player::CAN_GO_NEXT, conn)
            .await
    }

    pub async fn can_go_previous(&self, conn: &Connection) -> Result<bool> {
        self.get(
            &mpris::player::INTERFACE,
            &mpris::player::CAN_GO_PREVIOUS,
            conn,
        )
        .await
    }

    pub async fn can_play(&self, conn: &Connection) -> Result<bool> {
        self.get(&mpris::player::INTERFACE, &mpris::player::CAN_PLAY, conn)
            .await
    }

    pub async fn can_pause(&self, conn: &Connection) -> Result<bool> {
        self.get(&mpris::player::INTERFACE, &mpris::player::CAN_PAUSE, conn)
            .await
    }

    pub async fn can_seek(&self, conn: &Connection) -> Result<bool> {
        self.get(&mpris::player::INTERFACE, &mpris::player::CAN_SEEK, conn)
            .await
    }

    pub async fn can_control(&self, conn: &Connection) -> Result<bool> {
        self.get(&mpris::player::INTERFACE, &mpris::player::CAN_CONTROL, conn)
            .await
    }

    //////// Empress-specific wrapper methods ////////

    pub async fn try_next(self, conn: &Connection) -> Result<Option<Self>> {
        Ok(if self.can_go_next(conn).await? {
            Some(self.next(conn).await?)
        } else {
            None
        })
    }

    pub async fn try_previous(self, conn: &Connection) -> Result<Option<Self>> {
        Ok(if self.can_go_previous(conn).await? {
            Some(self.previous(conn).await?)
        } else {
            None
        })
    }

    pub async fn try_pause(self, conn: &Connection) -> Result<Option<Self>> {
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

    pub async fn try_play_pause(self, conn: &Connection) -> Result<Option<Self>> {
        Ok(match self.playback_status(conn).await? {
            PlaybackStatus::Playing if self.can_pause(conn).await? => Some(self.pause(conn).await?),
            PlaybackStatus::Paused if self.can_play(conn).await? => Some(self.play(conn).await?),
            _ => None,
        })
    }

    pub async fn try_stop(self, conn: &Connection) -> Result<Option<Self>> {
        Ok(match self.playback_status(conn).await? {
            PlaybackStatus::Playing | PlaybackStatus::Paused if self.can_control(conn).await? => {
                Some(self.stop(conn).await?)
            },
            _ => None,
        })
    }

    pub async fn try_play(self, conn: &Connection) -> Result<Option<Self>> {
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

    #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation)]
    pub async fn try_seek(self, conn: &Connection, to: Offset) -> Result<Option<(Self, f64)>> {
        Ok(if self.can_seek(conn).await? {
            let meta = self.metadata(conn).await?;

            let pos = match to {
                Offset::Relative(p) => self.position(conn).await? + (p * 1e6).round() as i64,
                Offset::Absolute(p) => (p * 1e6).round() as i64,
            };

            Some((
                self.set_position(
                    conn,
                    meta.get(mpris::track_list::ATTR_TRACKID)
                        .ok_or_else(|| anyhow!("missing track ID in metadata"))?
                        .downcast_ref::<ObjectPath>()
                        .map(ObjectPath::as_ref)
                        .ok_or_else(|| anyhow!("track ID wasn't a valid path"))?,
                    pos,
                )
                .await?,
                pos as f64 / 1e6,
            ))
        } else {
            None
        })
    }

    pub async fn try_set_volume(
        self,
        conn: &Connection,
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
