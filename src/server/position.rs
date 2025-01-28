use std::time::Instant;

use spin::mutex::SpinMutex;

#[derive(Debug, Default)]
pub struct PositionCache(SpinMutex<Option<Position>>);

impl PositionCache {
    pub fn get(&self) -> Option<Position> { *self.0.lock() }

    pub fn try_update_status(
        &self,
        playing: Option<bool>,
        rate: Option<f64>,
        at: impl Into<Option<Instant>>,
    ) {
        if let Some(pos) = &mut *self.0.lock() {
            pos.update_status(playing, rate, at);
        }
    }

    pub fn force_seek(
        &self,
        micros: i64,
        playing: bool,
        rate: f64,
        at: impl Into<Option<Instant>>,
    ) -> Position {
        let mut pos = self.0.lock();
        if let Some(pos) = &mut *pos {
            pos.seek(micros, at);

            *pos
        } else {
            let p = at.into().map_or_else(
                || Position::capture(micros, playing, rate),
                |a| Position::new(micros, playing, rate, a),
            );
            *pos = Some(p);
            p
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    micros: i64,
    playing: bool,
    rate: f64,
    captured: Instant,
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        let ts = Instant::now();
        self.get(ts).eq(&other.get(ts))
    }
}

impl Position {
    #[inline]
    pub const fn new(micros: i64, playing: bool, rate: f64, captured: Instant) -> Self {
        Self {
            micros,
            playing,
            rate,
            captured,
        }
    }

    #[inline]
    pub fn capture(micros: i64, playing: bool, rate: f64) -> Self {
        Self::new(micros, playing, rate, Instant::now())
    }

    #[inline]
    pub fn rate(self) -> f64 {
        if self.playing {
            self.rate
        } else {
            0.0
        }
    }

    pub fn get(self, at: impl Into<Option<Instant>>) -> i64 {
        let now = at.into().unwrap_or_else(Instant::now);
        #[expect(
            clippy::cast_possible_truncation,
            reason = "TryFrom<{float}> for {int} doesn't exist yet :("
        )]
        self.micros.saturating_add(
            ((now - self.captured).as_secs_f64() * self.rate() * 1e6).round() as i64,
        )
    }

    pub fn update_status(
        &mut self,
        playing: Option<bool>,
        rate: Option<f64>,
        at: impl Into<Option<Instant>>,
    ) -> &mut Self {
        let now = at.into().unwrap_or_else(Instant::now);

        if now < self.captured {
            return self;
        }

        *self = Self::new(
            self.get(now),
            playing.unwrap_or(self.playing),
            rate.unwrap_or(self.rate),
            now,
        );
        self
    }

    pub fn seek(&mut self, micros: i64, at: impl Into<Option<Instant>>) -> &mut Self {
        let now = at.into().unwrap_or_else(Instant::now);

        if now < self.captured {
            return self;
        }

        self.micros = micros;
        self.captured = now;
        self
    }
}
