use std::{
    future::Future,
    io::{self, IsTerminal},
    time::Duration,
};

use anyhow::{Context, Error};
use futures_util::StreamExt;
use jiff::civil::DateTime;
use log::{info, trace, warn};
use serde::Serialize;
use zbus::{
    connection,
    proxy::PropertyChanged,
    zvariant::{ObjectPath, OwnedValue},
};

use self::proxy::EmpressProxy;
use crate::{
    format,
    server::{
        self,
        mpris::{self, player::PlaybackStatus},
        MatchPlayer, PlayerStatus, PlayerStatusKind, Position,
    },
    timeout::Timeout,
    ClientCommand, Offset, PlayerOpts, Result, SERVER_NAME,
};

mod proxy;

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct NowPlayingPlayer {
    bus: Option<String>,
    id: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
struct NowPlayingResult {
    status: PlaybackStatus,
    player: NowPlayingPlayer,
    rate: f64,
    position: Option<i64>,

    track_id: Option<String>,
    length: Option<i64>,
    art_url: Option<String>,

    album: Option<String>,
    album_artists: Option<Vec<String>>,
    artists: Option<Vec<String>>,
    lyrics: Option<String>,
    bpm: Option<i64>,
    auto_rating: Option<f64>,
    comments: Option<Vec<String>>,
    composers: Option<Vec<String>>,
    date_created: Option<DateTime>,
    disc_num: Option<i64>,
    date_first_played: Option<DateTime>,
    genres: Option<Vec<String>>,
    date_last_played: Option<DateTime>,
    lyricists: Option<Vec<String>>,
    title: Option<String>,
    track_num: Option<i64>,
    url: Option<String>,
    play_count: Option<i64>,
    user_rating: Option<f64>,
}

impl NowPlayingResult {
    fn capture_position(&self) -> Option<Position> {
        self.position
            .map(|p| Position::capture(p, self.status.is_playing(), self.rate))
    }
}

macro_rules! extract {
    ($meta:ident { $($key:ident => $var:ident ($conv:expr)),* $(,)? }) => {
        $(
            let $var = $meta
                .remove(mpris::track_list::$key)
                .and_then(|v| {
                    $conv(v)
                        .context(concat!(
                            "Error parsing field \"",
                            stringify!($var),
                            "\" of player status",
                        ))
                        .map_err(|e| warn!("{:?}", e))
                        .ok()
                });
        )*
    };
}

#[allow(clippy::needless_pass_by_value)]
fn parse_path(val: OwnedValue) -> Result<String, zbus::zvariant::Error> {
    val.downcast_ref::<ObjectPath>()
        .map(|p| p.to_string())
        .or_else(|_| String::try_from(val))
}

#[allow(clippy::needless_pass_by_value)]
fn parse_i64(val: OwnedValue) -> Result<i64> {
    i64::try_from(&val)
        .or_else(|_| i32::try_from(&val).map(Into::into))
        .context("Error downcasting i64/i32")
        .or_else(|_| {
            u64::try_from(&val)
                .context("Error downcasting u64")
                .and_then(|u| u.try_into().context("Error converting u64 to i64"))
        })
}

#[allow(clippy::needless_pass_by_value)]
fn parse_datetime(val: OwnedValue) -> Result<DateTime> {
    let s = val
        .downcast_ref::<&str>()
        .context("Unable to downcast value to string")?;
    let tz = jiff::tz::TimeZone::system();

    s.parse::<jiff::Timestamp>()
        .map(|t| t.to_zoned(tz.clone()).datetime())
        .or_else(|_| {
            s.parse::<jiff::Zoned>()
                .map(|z| z.with_time_zone(tz).datetime())
        })
        .or_else(|_| s.parse::<DateTime>())
        .context("Error parsing date string")
}

impl TryFrom<PlayerStatus> for NowPlayingResult {
    type Error = Error;

    fn try_from(status: PlayerStatus) -> Result<Self> {
        let PlayerStatus {
            kind,
            bus,
            ident,
            status,
            rate,
            position,
            mut metadata,
        } = status;

        let (bus, id, position) = match kind {
            PlayerStatusKind::NoPlayer => (None, None, None),
            PlayerStatusKind::NoPosition => (Some(bus), Some(ident), None),
            PlayerStatusKind::Default => (Some(bus), Some(ident), Some(position)),
        };

        extract!(metadata {
            ATTR_TRACK_ID => track_id(parse_path),
            ATTR_LENGTH => length(parse_i64),
            ATTR_ART_URL => art_url(String::try_from),

            ATTR_ALBUM => album(String::try_from),
            ATTR_ALBUM_ARTISTS => album_artists(Vec::try_from),
            ATTR_ARTISTS => artists(Vec::try_from),
            ATTR_LYRICS => lyrics(String::try_from),
            ATTR_BPM => bpm(parse_i64),
            ATTR_AUTO_RATING => auto_rating(f64::try_from),
            ATTR_COMMENTS => comments(Vec::try_from),
            ATTR_COMPOSERS => composers(Vec::try_from),
            ATTR_DATE_CREATED => date_created(parse_datetime),
            ATTR_DISC_NUM => disc_num(parse_i64),
            ATTR_DATE_FIRST_PLAYED => date_first_played(parse_datetime),
            ATTR_GENRES => genres(Vec::try_from),
            ATTR_DATE_LAST_PLAYED => date_last_played(parse_datetime),
            ATTR_LYRICISTS => lyricists(Vec::try_from),
            ATTR_TITLE => title(String::try_from),
            ATTR_TRACK_NUM => track_num(parse_i64),
            ATTR_URL => url(String::try_from),
            ATTR_PLAY_COUNT => play_count(parse_i64),
            ATTR_USER_RATING => user_rating(f64::try_from),
        });

        trace!("Unused metadata for status: {metadata:?}");

        Ok(Self {
            status,
            player: NowPlayingPlayer { bus, id },
            rate,
            position,
            track_id,
            length,
            art_url,
            album,
            album_artists,
            artists,
            lyrics,
            bpm,
            auto_rating,
            comments,
            composers,
            date_created,
            disc_num,
            date_first_played,
            genres,
            date_last_played,
            lyricists,
            title,
            track_num,
            url,
            play_count,
            user_rating,
        })
    }
}

impl MatchPlayer for NowPlayingResult {
    fn bus(&self) -> &str { self.player.bus.as_ref().map_or("", |s| s) }

    fn status(&self) -> PlaybackStatus { self.status }
}

macro_rules! courtesy_line {
    () => {
        if std::io::stdout().is_terminal() {
            println!();
        }
    };
}

#[allow(clippy::too_many_lines)]
pub(super) async fn run(cmd: ClientCommand) -> Result {
    let conn = connection::Builder::session()
        .context("Error creatihng session connection builder")?
        .build()
        .await
        .context("Error connecting to D-Bus")?;

    let proxy = Timeout::from(
        proxy::EmpressProxy::builder(&conn)
            .destination(&*SERVER_NAME)
            .context("Error setting empress proxy destination")?
            .build()
            .await
            .context("Error building server proxy")?,
    );

    match cmd {
        ClientCommand::Scan => {
            let log = try_send(&proxy, EmpressProxy::scan).await?;

            if log.is_empty() {
                info!("No changes detected");
            } else {
                for line in log {
                    warn!("Change detected: {line}");
                }
            }
        },
        ClientCommand::Raise(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.raise(&opts)).await?;
        },
        ClientCommand::Next(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.next(&opts)).await?;
        },
        ClientCommand::Previous(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.prev(&opts)).await?;
        },
        ClientCommand::Pause(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.pause(&opts)).await?;
        },
        ClientCommand::PlayPause(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.play_pause(&opts)).await?;
        },
        ClientCommand::Stop(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.stop(&opts)).await?;
        },
        ClientCommand::Play(opts) => {
            let opts = opts.into();
            try_send(&proxy, |p| p.play(&opts)).await?;
        },
        ClientCommand::ListPlayers => {
            let players = try_send(&proxy, EmpressProxy::list_players).await?;

            for (player, status) in players {
                println!("{player}\t{status}");
            }
        },
        ClientCommand::NowPlaying {
            player,
            format,
            watch,
            zero,
        } => now_playing(proxy, player, format, watch, zero).await?,
        ClientCommand::Seek {
            player,
            to: Offset::Relative(to),
        } => {
            let player = player.into();
            try_send(&proxy, |p| p.seek_relative(&player, to)).await?;
        },
        ClientCommand::Seek {
            player,
            to: Offset::Absolute(to),
        } => {
            let player = player.into();
            try_send(&proxy, |p| p.seek_absolute(&player, to)).await?;
        },
        ClientCommand::Volume {
            player,
            vol: Offset::Relative(to),
        } => {
            let player = player.into();
            let vol = try_send(&proxy, |p| p.vol_relative(&player, to)).await?;

            print!("{vol}");
            courtesy_line!();
        },
        ClientCommand::Volume {
            player,
            vol: Offset::Absolute(to),
        } => {
            let player = player.into();
            let vol = try_send(&proxy, |p| p.vol_absolute(&player, to)).await?;

            print!("{vol}");
            courtesy_line!();
        },
        ClientCommand::SwitchCurrent { to, no_play } => {
            let switch_playing = !no_play;
            try_send(&proxy, |p| p.switch_current(&to, switch_playing)).await?;
        },
    }

    Ok(())
}

fn now_playing_sep(zero: bool) -> char {
    if zero {
        '\0'
    } else {
        '\n'
    }
}

async fn now_playing(
    proxy: Timeout<EmpressProxy<'_>>,
    player: PlayerOpts,
    format: Option<String>,
    watch: bool,
    zero: bool,
) -> Result {
    let player = player.into();

    let status = try_send(&proxy, |p| async {
        if player == server::PlayerOpts::default() {
            p.now_playing().await
        } else {
            p.player_status(&player).await
        }
    })
    .await?;

    trace!("Full now-playing response: {status:?}");
    let status = status.try_into()?;
    let format = format.as_deref();
    let last = print_now_playing(
        &status,
        format,
        watch.then(|| now_playing_sep(zero)),
        None,
        || (),
    )?;

    if watch {
        watch_now_playing(proxy, player, format, zero, status, last).await?;
    } else {
        courtesy_line!();
    }

    Ok(())
}

async fn watch_now_playing(
    proxy: Timeout<EmpressProxy<'_>>,
    player: server::PlayerOpts,
    format: Option<&str>,
    zero: bool,
    mut status: NowPlayingResult,
    mut last: String,
) -> Result<()> {
    enum Event<'a> {
        TickSec,
        TickThrottled,
        Update(Option<PropertyChanged<'a, PlayerStatus>>),
        Stop(Result<(), io::Error>),
    }

    const MICROS_PER_SEC: i64 = 1_000_000;
    const THROTTLE_MILLIS: u32 = 60;

    fn sep(zero: bool) {
        use std::io::prelude::*;
        let mut io = std::io::stdout();
        write!(io, "{}", now_playing_sep(zero)).unwrap();
        io.flush().unwrap();
    }

    async fn tick(status: &NowPlayingResult) -> Option<Event<'static>> {
        if !status.status.is_playing() {
            return None;
        }

        let rem_micros = 1_000_000 - status.position? % MICROS_PER_SEC;

        #[expect(
            clippy::cast_precision_loss,
            reason = "Precision loss should not be an issue since rem_micros is [0, 1e6)"
        )]
        let mut sleep_millis = rem_micros as f64 / (status.rate * 1e3).min(1e7);
        // I'm sure there's a way to make this branchless without worrying
        // about whether the compiler will do it, but NaN scares me so it's in
        // no way worth it
        if !sleep_millis.is_finite() {
            sleep_millis = 0.0;
        }
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            reason = "Casting the ceiling of sleep_millis to u64 should not lose any critical \
                      information"
        )]
        let sleep_millis = sleep_millis.ceil() as u64;

        if sleep_millis > THROTTLE_MILLIS.into() {
            tokio::time::sleep(Duration::from_millis(sleep_millis)).await;
            Some(Event::TickSec)
        } else {
            tokio::time::sleep(Duration::from_millis(THROTTLE_MILLIS.into())).await;
            Some(Event::TickThrottled)
        }
    }

    sep(zero);

    let mut position = status.capture_position();

    let player = player.build().context("Invalid player filter options")?;
    let mut stream = proxy
        .run(
            Duration::from_secs(1),
            EmpressProxy::receive_now_playing_changed,
        )
        .await
        .context("Error getting now-playing status")?;

    loop {
        let event = tokio::select! {
            Some(e) = tick(&status) => e,
            s = stream.next() => Event::Update(s),
            r = tokio::signal::ctrl_c() => Event::Stop(r),
        };

        match event {
            Event::TickSec => {
                if let Some(pos) = position {
                    let last = status.position;
                    let curr = pos.get(None);
                    let curr_rem = curr % MICROS_PER_SEC;
                    // Make sure the seconds
                    status.position = Some(
                        if last.is_some_and(|l| l - l % MICROS_PER_SEC == curr - curr_rem) {
                            curr + MICROS_PER_SEC - curr_rem
                        } else {
                            curr
                        },
                    );
                }
            },
            Event::TickThrottled => status.position = position.map(|p| p.get(None)),
            Event::Update(Some(s)) => {
                let next: NowPlayingResult = Timeout::from(s)
                    .try_run(Duration::from_secs(1), PropertyChanged::get)
                    .await
                    .context("Error parsing property value")?
                    .try_into()?;

                if next == status || !player.is_match(&next) {
                    continue;
                }

                trace!("Full now-playing response: {status:?}");
                status = next;
                position = status.capture_position();
            },
            Event::Update(None) => break Err(anyhow::anyhow!("Empress server hung up")),
            Event::Stop(r) => break r.context("Error catching ^C"),
        }

        last = print_now_playing(
            &status,
            format,
            Some(now_playing_sep(zero)),
            Some(last),
            || sep(zero),
        )?;
    }
}

#[inline]
fn print_now_playing(
    status: &NowPlayingResult,
    format: Option<&str>,
    sep: Option<char>,
    last: Option<String>,
    post_print: impl FnOnce(),
) -> Result<String> {
    let mut s = if let Some(format) = format {
        format::eval(format, status)?
    } else {
        serde_json::to_string(status)?
    };

    if let Some(c) = sep {
        s = s.replace(c, " ");
    }

    if last.is_none_or(|l| l != s) {
        print!("{s}");
        post_print();
    }

    Ok(s)
}

async fn try_send<
    'a,
    T: 'a,
    F: Fn(&'a T) -> FR,
    FR: Future<Output = zbus::fdo::Result<R>> + 'a,
    R,
>(
    with: &'a Timeout<T>,
    call: F,
) -> Result<R> {
    const MAX_TRIES: usize = 5;

    let mut i = 0;

    loop {
        match with.try_run(Duration::from_secs(2), &call).await {
            Err(e) if i < MAX_TRIES => warn!("Request failed: {e}"),
            r => break r.context("Unable to contact empress server"),
        }

        i += 1;
        info!("Retry attempt {i}");

        tokio::time::sleep(Duration::from_millis(20)).await;
        assert!(i <= MAX_TRIES);
    }
}
