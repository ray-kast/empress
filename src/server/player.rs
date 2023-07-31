use std::{collections::HashMap, fmt::Debug, time::Instant};

use anyhow::{anyhow, Context};
use zbus::{
    names::{BusName, WellKnownName},
    zvariant::{ObjectPath, OwnedValue},
    Connection,
};

use super::{
    mpris,
    mpris::{player::PlaybackStatus, MediaPlayerProxy, PlayerProxy},
};
use crate::{timeout::Timeout, Offset, Result};

#[derive(Debug, Clone)]
pub(super) struct Player {
    status: PlaybackStatus,
    last_update: Instant,
    mp2: Timeout<MediaPlayerProxy<'static>>,
    player: Timeout<PlayerProxy<'static>>,
}

#[inline]
async fn timeout<
    'a,
    T: 'a,
    F: FnOnce(&'a T) -> FR + 'a,
    FR: std::future::Future<Output = Result<R, E>>,
    R,
    E,
>(
    t: &'a Timeout<T>,
    f: F,
) -> Result<R, crate::timeout::Error<E>> {
    t.try_run(std::time::Duration::from_secs(2), f).await
}

impl Player {
    pub async fn new(
        now: Instant,
        name: impl Into<BusName<'static>> + Clone,
        conn: &Connection,
    ) -> Result<Self> {
        let mut ret = Self {
            status: PlaybackStatus::Stopped,
            last_update: now,
            mp2: MediaPlayerProxy::builder(conn)
                .destination(name.clone())
                .context("Error setting MediaPlayer2 proxy destination")?
                .build()
                .await
                .context("Error building MediaPlayer2 proxy")?
                .into(),
            player: PlayerProxy::builder(conn)
                .destination(name)
                .context("Error setting player proxy destination")?
                .build()
                .await
                .context("Error building player proxy")?
                .into(),
        };

        ret.refresh(now).await?;

        Ok(ret)
    }

    pub async fn refresh(&mut self, now: Instant) -> Result<bool> {
        let next_status = self.playback_status().await?;
        let ret = self.status == next_status;
        self.status = next_status;

        if ret {
            self.last_update = now;
        }

        Ok(ret)
    }

    //////// Accessors ////////

    #[inline]
    pub fn status(&self) -> PlaybackStatus {
        self.status
    }

    #[inline]
    pub fn update_status(&mut self, status: PlaybackStatus) -> Option<Instant> {
        if self.status == status {
            return None;
        }

        let now = Instant::now();
        self.status = status;
        self.last_update = now;
        Some(now)
    }

    #[inline]
    pub fn last_update(&self) -> Instant {
        self.last_update
    }

    #[inline]
    pub fn force_update(&mut self) -> Instant {
        let now = Instant::now();
        self.last_update = now;
        now
    }

    #[inline]
    pub fn bus(&self) -> &WellKnownName {
        let player_dest = unsafe { self.player.smuggle(|p| p.destination()) };
        debug_assert!(self.mp2.block(|m| m.destination() == player_dest));
        match player_dest {
            BusName::Unique(u) => unreachable!("MPRIS bus had unique name {:?}", u.as_str()),
            BusName::WellKnown(w) => w,
        }
    }

    //////// Methods under MediaPlayer2.Player ////////

    pub async fn next(self) -> Result<Self> {
        timeout(&self.player, PlayerProxy::next)
            .await
            .context("Proxy call for Next failed")?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn previous(self) -> Result<Self> {
        timeout(&self.player, PlayerProxy::previous)
            .await
            .context("Proxy call for Previous failed")?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn pause(self) -> Result<Self> {
        timeout(&self.player, PlayerProxy::pause)
            .await
            .context("Proxy call for Pause failed")?;

        Ok(Self {
            status: PlaybackStatus::Paused,
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn stop(self) -> Result<Self> {
        timeout(&self.player, PlayerProxy::stop)
            .await
            .context("Proxy call for Stop failed")?;

        Ok(Self {
            status: PlaybackStatus::Stopped,
            last_update: Instant::now(),
            ..self
        })
    }

    pub async fn play(self) -> Result<Self> {
        timeout(&self.player, PlayerProxy::play)
            .await
            .context("Proxy call for Play failed")?;

        Ok(Self {
            status: PlaybackStatus::Playing,
            last_update: Instant::now(),
            ..self
        })
    }

    #[allow(clippy::cast_possible_truncation)]
    pub async fn set_position(self, id: ObjectPath<'_>, secs: i64) -> Result<Self> {
        timeout(&self.player, |p| p.set_position(id, secs))
            .await
            .context("Proxy call for SetPosition failed")?;

        Ok(Self {
            last_update: Instant::now(),
            ..self
        })
    }

    //////// Properties under MediaPlayer2 ////////

    pub async fn identity(&self) -> Result<String> {
        timeout(&self.mp2, MediaPlayerProxy::identity)
            .await
            .context("Proxy property get for Identity failed")
    }

    //////// Properties under MediaPlayer2.Player ////////

    pub async fn playback_status(&self) -> Result<PlaybackStatus> {
        timeout(&self.player, PlayerProxy::playback_status)
            .await
            .context("Proxy property get for PlaybackStatus failed")
    }

    pub async fn metadata(&self) -> Result<HashMap<String, OwnedValue>> {
        timeout(&self.player, PlayerProxy::metadata)
            .await
            .context("Proxy property get for Metadata failed")
    }

    pub async fn volume(&self) -> Result<f64> {
        timeout(&self.player, PlayerProxy::volume)
            .await
            .context("Proxy property get for Volume failed")
    }

    pub async fn set_volume(&self, vol: f64) -> Result<()> {
        timeout(&self.player, |p| p.set_volume(vol))
            .await
            .context("Proxy property set for Volume failed")
    }

    pub async fn position(&self) -> Result<i64> {
        timeout(&self.player, PlayerProxy::position)
            .await
            .context("Proxy property get for Position failed")
    }

    pub async fn can_go_next(&self) -> Result<bool> {
        timeout(&self.player, PlayerProxy::can_go_next)
            .await
            .context("Proxy property get for CanGoNext failed")
    }

    pub async fn can_go_previous(&self) -> Result<bool> {
        timeout(&self.player, PlayerProxy::can_go_previous)
            .await
            .context("Proxy property get for CanGoPrevious failed")
    }

    pub async fn can_play(&self) -> Result<bool> {
        timeout(&self.player, PlayerProxy::can_play)
            .await
            .context("Proxy property get for CanPlay failed")
    }

    pub async fn can_pause(&self) -> Result<bool> {
        timeout(&self.player, PlayerProxy::can_pause)
            .await
            .context("Proxy property get for CanPause failed")
    }

    pub async fn can_seek(&self) -> Result<bool> {
        timeout(&self.player, PlayerProxy::can_seek)
            .await
            .context("Proxy property get for CanSeek failed")
    }

    pub async fn can_control(&self) -> Result<bool> {
        timeout(&self.player, PlayerProxy::can_control)
            .await
            .context("Proxy property get for CanControl failed")
    }

    //////// Empress-specific wrapper methods ////////

    pub async fn try_next(self) -> Result<Option<Self>> {
        Ok(if self.can_go_next().await? {
            Some(self.next().await?)
        } else {
            None
        })
    }

    pub async fn try_previous(self) -> Result<Option<Self>> {
        Ok(if self.can_go_previous().await? {
            Some(self.previous().await?)
        } else {
            None
        })
    }

    pub async fn try_pause(self) -> Result<Option<Self>> {
        Ok(
            if self.playback_status().await? == PlaybackStatus::Playing && self.can_pause().await? {
                Some(self.pause().await?)
            } else {
                None
            },
        )
    }

    pub async fn try_play_pause(self) -> Result<Option<Self>> {
        Ok(match self.playback_status().await? {
            PlaybackStatus::Playing if self.can_pause().await? => Some(self.pause().await?),
            PlaybackStatus::Paused if self.can_play().await? => Some(self.play().await?),
            _ => None,
        })
    }

    pub async fn try_stop(self) -> Result<Option<Self>> {
        Ok(match self.playback_status().await? {
            PlaybackStatus::Playing | PlaybackStatus::Paused if self.can_control().await? => {
                Some(self.stop().await?)
            },
            _ => None,
        })
    }

    pub async fn try_play(self) -> Result<Option<Self>> {
        Ok(
            if self.playback_status().await? != PlaybackStatus::Playing && self.can_play().await? {
                Some(self.play().await?)
            } else {
                None
            },
        )
    }

    #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation)]
    pub async fn try_seek(self, to: Offset) -> Result<Option<(Self, f64)>> {
        Ok(if self.can_seek().await? {
            let meta = self.metadata().await?;

            let pos = match to {
                Offset::Relative(p) => self.position().await? + (p * 1e6).round() as i64,
                Offset::Absolute(p) => (p * 1e6).round() as i64,
            };

            Some((
                self.set_position(
                    meta.get(mpris::track_list::ATTR_TRACKID)
                        .ok_or_else(|| anyhow!("Missing track ID in metadata"))?
                        .downcast_ref::<ObjectPath>()
                        .map(ObjectPath::as_ref)
                        .ok_or_else(|| anyhow!("Track ID wasn't a valid path"))?,
                    pos,
                )
                .await?,
                pos as f64 / 1e6,
            ))
        } else {
            None
        })
    }

    pub async fn try_set_volume(self, vol: Offset) -> Result<Option<(Self, f64)>> {
        let (vol, set) = match vol {
            Offset::Relative(v) => {
                let prev = self.volume().await?;
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
            return Err(anyhow!("Invalid volume {vol:?}"));
        }

        Ok(if set {
            if self.can_control().await? {
                // Safety check
                let vol = vol.clamp(0.0, 1.0);

                self.set_volume(vol).await?;

                Some((self, vol))
            } else {
                None
            }
        } else {
            Some((self, vol))
        })
    }
}
