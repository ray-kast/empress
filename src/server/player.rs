use std::{collections::HashMap, fmt::Debug, future::Future, time::Instant};

use anyhow::{anyhow, Context};
use zbus::{
    names::{BusName, WellKnownName},
    zvariant::{ObjectPath, OwnedValue},
    Connection,
};

use super::{
    mpris::{self, player::PlaybackStatus, MediaPlayerProxy, PlayerProxy},
    position::PositionCache,
    MatchPlayer, Position,
};
use crate::{timeout::Timeout, Offset, Result};

pub(super) trait Action {
    type Arg;
    type Output;

    fn can_run(&self, player: &Player) -> impl Future<Output = Result<Option<Self::Arg>>>;

    fn run(
        &self,
        player: &mut Player,
        arg: Self::Arg,
    ) -> impl Future<Output = Result<Self::Output>>;
}

#[derive(Debug)]
pub(super) struct Player {
    status: PlaybackStatus,
    last_update: Instant,
    position: PositionCache,
    mp2: Timeout<MediaPlayerProxy<'static>>,
    inner: Timeout<PlayerProxy<'static>>,
}

#[inline]
async fn timeout<
    'a,
    T: 'a,
    F: FnOnce(&'a T) -> FR + 'a,
    FR: std::future::Future<Output = Result<R, E>> + 'a,
    R,
    E,
>(
    t: &'a Timeout<T>,
    f: F,
) -> Result<R, crate::timeout::Error<E>> {
    t.try_run(std::time::Duration::from_secs(2), f).await
}

macro_rules! updated_now {
    ($self:expr) => {{
        $self.last_update = Instant::now();
        Ok(())
    }};
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
            position: PositionCache::default(),
            mp2: MediaPlayerProxy::builder(conn)
                .destination(name.clone())
                .context("Error setting MediaPlayer2 proxy destination")?
                .build()
                .await
                .context("Error building MediaPlayer2 proxy")?
                .into(),
            inner: PlayerProxy::builder(conn)
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
    pub fn status(&self) -> PlaybackStatus { self.status }

    #[inline]
    pub fn update_status(&mut self, status: PlaybackStatus) -> Option<Instant> {
        if self.status == status {
            return None;
        }

        let now = Instant::now();
        self.status = status;
        self.last_update = now;
        self.position
            .try_update_status(Some(status.is_playing()), None, now);
        Some(now)
    }

    #[inline]
    pub fn last_update(&self) -> Instant { self.last_update }

    #[inline]
    pub fn force_update(&mut self) -> Instant {
        let now = Instant::now();
        self.last_update = now;
        now
    }

    pub async fn force_update_position(&self, micros: Option<i64>) -> Result<Position> {
        let now = Instant::now();
        let micros = if let Some(micros) = micros {
            micros
        } else {
            timeout(&self.inner, PlayerProxy::position)
                .await
                .context("Proxy property get for Position failed")?
        };
        let rate = self.rate().await?;

        Ok(self
            .position
            .force_seek(micros, self.status.is_playing(), rate, now))
    }

    pub fn update_rate(&self, rate: f64) {
        self.position.try_update_status(None, Some(rate), None);
    }

    #[inline]
    pub fn bus(&self) -> &WellKnownName {
        let player_dest = unsafe { self.inner.smuggle(|p| p.inner().destination()) };
        debug_assert!(self.mp2.block(|m| m.inner().destination() == player_dest));
        match player_dest {
            BusName::Unique(u) => unreachable!("MPRIS bus had unique name {:?}", u.as_str()),
            BusName::WellKnown(w) => w,
        }
    }

    //////// Methods under MediaPlayer2 ////////

    pub async fn raise(&mut self) -> Result {
        timeout(&self.mp2, MediaPlayerProxy::raise)
            .await
            .context("Proxy call for Raise failed")?;

        Ok(())
    }

    //////// Methods under MediaPlayer2.Player ////////

    pub async fn next(&mut self) -> Result {
        timeout(&self.inner, PlayerProxy::next)
            .await
            .context("Proxy call for Next failed")?;

        updated_now!(self)
    }

    pub async fn previous(&mut self) -> Result {
        timeout(&self.inner, PlayerProxy::previous)
            .await
            .context("Proxy call for Previous failed")?;

        updated_now!(self)
    }

    pub async fn pause(&mut self) -> Result {
        timeout(&self.inner, PlayerProxy::pause)
            .await
            .context("Proxy call for Pause failed")?;

        self.status = PlaybackStatus::Paused;
        updated_now!(self)
    }

    pub async fn stop(&mut self) -> Result {
        timeout(&self.inner, PlayerProxy::stop)
            .await
            .context("Proxy call for Stop failed")?;

        self.status = PlaybackStatus::Stopped;
        updated_now!(self)
    }

    pub async fn play(&mut self) -> Result {
        timeout(&self.inner, PlayerProxy::play)
            .await
            .context("Proxy call for Play failed")?;

        self.status = PlaybackStatus::Playing;
        updated_now!(self)
    }

    #[allow(clippy::cast_possible_truncation)]
    pub async fn set_position(&mut self, id: ObjectPath<'_>, micros: i64) -> Result {
        timeout(&self.inner, |p| p.set_position(id, micros))
            .await
            .context("Proxy call for SetPosition failed")?;

        updated_now!(self)
    }

    //////// Properties under MediaPlayer2 ////////

    pub async fn can_raise(&self) -> Result<bool> {
        timeout(&self.mp2, MediaPlayerProxy::can_raise)
            .await
            .context("Proxy property get for CanRaise failed")
    }

    pub async fn identity(&self) -> Result<String> {
        timeout(&self.mp2, MediaPlayerProxy::identity)
            .await
            .context("Proxy property get for Identity failed")
    }

    //////// Properties under MediaPlayer2.Player ////////

    pub async fn playback_status(&self) -> Result<PlaybackStatus> {
        timeout(&self.inner, PlayerProxy::playback_status)
            .await
            .context("Proxy property get for PlaybackStatus failed")
    }

    pub async fn rate(&self) -> Result<f64> {
        timeout(&self.inner, PlayerProxy::rate)
            .await
            .context("Proxy property get for Rate failed")
    }

    pub async fn metadata(&self) -> Result<HashMap<String, OwnedValue>> {
        timeout(&self.inner, PlayerProxy::metadata)
            .await
            .context("Proxy property get for Metadata failed")
    }

    pub async fn volume(&self) -> Result<f64> {
        timeout(&self.inner, PlayerProxy::volume)
            .await
            .context("Proxy property get for Volume failed")
    }

    pub async fn set_volume(&self, vol: f64) -> Result<()> {
        timeout(&self.inner, |p| p.set_volume(vol))
            .await
            .context("Proxy property set for Volume failed")
    }

    pub async fn position(&self) -> Result<Position> {
        if let Some(pos) = self.position.get() {
            Ok(pos)
        } else {
            self.force_update_position(None).await
        }
    }

    pub async fn can_go_next(&self) -> Result<bool> {
        timeout(&self.inner, PlayerProxy::can_go_next)
            .await
            .context("Proxy property get for CanGoNext failed")
    }

    pub async fn can_go_previous(&self) -> Result<bool> {
        timeout(&self.inner, PlayerProxy::can_go_previous)
            .await
            .context("Proxy property get for CanGoPrevious failed")
    }

    pub async fn can_play(&self) -> Result<bool> {
        timeout(&self.inner, PlayerProxy::can_play)
            .await
            .context("Proxy property get for CanPlay failed")
    }

    pub async fn can_pause(&self) -> Result<bool> {
        timeout(&self.inner, PlayerProxy::can_pause)
            .await
            .context("Proxy property get for CanPause failed")
    }

    pub async fn can_seek(&self) -> Result<bool> {
        timeout(&self.inner, PlayerProxy::can_seek)
            .await
            .context("Proxy property get for CanSeek failed")
    }

    pub async fn can_control(&self) -> Result<bool> {
        timeout(&self.inner, PlayerProxy::can_control)
            .await
            .context("Proxy property get for CanControl failed")
    }

    //////// Empress-specific wrapper methods ////////

    #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation)]
    async fn offset_position(&mut self, pos: Offset) -> Result<f64> {
        let meta = self.metadata().await?;

        let pos = match pos {
            Offset::Relative(p) => self.position().await?.get(None) + (p * 1e6).round() as i64,
            Offset::Absolute(p) => (p * 1e6).round() as i64,
        };

        self.set_position(
            meta.get(mpris::track_list::ATTR_TRACK_ID)
                .context("Missing track ID in metadata")?
                .downcast_ref::<&ObjectPath>()
                .map(ObjectPath::as_ref)?,
            pos,
        )
        .await?;

        Ok(pos as f64 / 1e6)
    }

    async fn offset_volume(&mut self, vol: Offset) -> Result<f64> {
        let (vol, set) = match vol {
            Offset::Relative(v) => {
                let old = self.volume().await?;
                let new = old + v;

                if (new - old).abs() > 1e-5 {
                    (new, true)
                } else {
                    (old, false)
                }
            },
            Offset::Absolute(v) => (v, true),
        };

        if !vol.is_finite() {
            return Err(anyhow!("Invalid volume {vol:?}"));
        }

        Ok(if set {
            // Safety check
            let vol = vol.clamp(0.0, 1.0);

            self.set_volume(vol).await?;

            vol
        } else {
            vol
        })
    }
}

impl MatchPlayer for Player {
    fn bus(&self) -> &str {
        self.bus()
            .strip_prefix(mpris::BUS_NAME.as_str())
            .and_then(|s| s.strip_prefix('.'))
            .unwrap_or("")
    }

    fn status(&self) -> PlaybackStatus { self.status }
}

trait IntoOption {
    type Output;

    fn into_option(self) -> Option<Self::Output>;
}

impl IntoOption for bool {
    type Output = ();

    #[inline]
    fn into_option(self) -> Option<Self::Output> { self.then_some(()) }
}

impl<T> IntoOption for Option<T> {
    type Output = T;

    #[inline]
    fn into_option(self) -> Option<Self::Output> { self }
}

macro_rules! action {
    (
        $vis:vis $name:ident $(($($inp:ty),* $(,)?))?: fn($($parm:ty),*) -> $output:ty,
        |$cr_me:pat_param, $cr_player:ident| $can_run:expr,
        |$r_me:pat_param, $r_player:ident, $r_arg:pat_param| $run:expr $(,)?
    ) => {
        #[derive(Clone, Copy)]
        $vis struct $name $(($(pub $inp,)*))?;

        impl Action for $name {
            type Arg = ($($parm,)*);
            type Output = $output;

            async fn can_run(&self, $cr_player: &Player) -> Result<Option<Self::Arg>> {
                let $cr_me = self;
                Ok(IntoOption::into_option($can_run))
            }

            async fn run(
                &self,
                $r_player: &mut Player,
                $r_arg: Self::Arg
            ) -> Result<Self::Output> {
                let $r_me = self;
                $run
            }
        }
    };
}

action!(
    pub Raise: fn() -> (),
    |Self, p| p.can_raise().await?,
    |Self, p, ()| p.raise().await,
);
action!(
    pub Next: fn() -> (),
    |Self, p| p.can_go_next().await?,
    |Self, p, ()| p.next().await,
);
action!(
    pub Prev: fn() -> (),
    |Self, p| p.can_go_previous().await?,
    |Self, p, ()| p.previous().await,
);
action!(
    pub Pause: fn() -> (),
    |Self, p| p.playback_status().await?.can_pause() && p.can_pause().await?,
    |Self, p, ()| p.pause().await,
);
action!(
    pub PlayPause: fn(bool) -> (),
    |Self, p| match p.playback_status().await? {
        PlaybackStatus::Playing if p.can_pause().await? => Some((true,)),
        PlaybackStatus::Paused if p.can_play().await? => Some((false,)),
        _ => None,
    },
    |Self, p, (playing,)| if playing {
        p.pause().await
    } else {
        p.play().await
    }
);
action!(
    pub Stop: fn() -> (),
    |Self, p| p.playback_status().await?.can_stop() && p.can_control().await?,
    |Self, p, ()| p.stop().await,
);
action!(
    pub Play: fn() -> (),
    |Self, p| p.playback_status().await?.can_play() && p.can_play().await?,
    |Self, p, ()| p.play().await,
);
action!(
    pub Seek(Offset): fn() -> f64,
    |Self(_), p| p.can_seek().await?,
    |Self(pos), p, ()| p.offset_position(*pos).await,
);
action!(
    pub SetVolume(Offset): fn() -> f64,
    |Self(_), p| p.can_control().await?,
    |Self(vol), p, ()| p.offset_volume(*vol).await,
);
