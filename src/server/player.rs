use std::time::{Duration, Instant};

use anyhow::Context;
use dbus::{
    nonblock::{stdintf::org_freedesktop_dbus::Properties, Proxy, SyncConnection},
    strings::{BusName, Member},
};

use super::{mpris, mpris::player::PlaybackStatus};
use crate::Result;

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

    pub async fn next(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::NEXT, conn).await?;

        Ok(Self {
            status: self.status,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    pub async fn previous(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::PREVIOUS, conn).await?;

        Ok(Self {
            status: self.status,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    pub async fn pause(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::PAUSE, conn).await?;

        Ok(Self {
            status: PlaybackStatus::Paused,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    pub async fn stop(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::STOP, conn).await?;

        Ok(Self {
            status: PlaybackStatus::Stopped,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    pub async fn play(self, conn: &SyncConnection) -> Result<Self> {
        self.call(&*mpris::player::PLAY, conn).await?;

        Ok(Self {
            status: PlaybackStatus::Playing,
            last_update: Instant::now(),
            bus: self.bus,
        })
    }

    pub async fn playback_status(&self, conn: &SyncConnection) -> Result<PlaybackStatus> {
        self.get(&*mpris::player::PLAYBACK_STATUS, conn)
            .await
            .and_then(|s: String| s.parse())
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
