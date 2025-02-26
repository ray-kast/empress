use std::{borrow::Cow, fs, io, time::Duration};

use anyhow::{Context, Error};
use futures_util::StreamExt;
use jiff::civil::DateTime;
use log::{trace, warn};
use zbus::{
    fdo,
    proxy::PropertyChanged,
    zvariant::{ObjectPath, OwnedValue},
};

use super::proxy::EmpressProxy;
use crate::{
    format,
    opts::{FormatKind, NowPlayingFormat, NowPlayingOpts},
    server::{
        self,
        mpris::{self, player::PlaybackStatus},
        MatchPlayer, PlayerOpts, PlayerStatus, PlayerStatusKind, Position,
    },
    timeout::Timeout,
    Result,
};

#[derive(Debug, Default, Clone, PartialEq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct Player {
    bus: Option<String>,
    id: Option<String>,
}

#[derive(Debug, Default, Clone, PartialEq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct NowPlaying {
    player: Player,
    status: PlaybackStatus,
    volume: Option<f64>,
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

impl NowPlaying {
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

fn parse_path(val: OwnedValue) -> Result<String, zbus::zvariant::Error> {
    val.downcast_ref::<ObjectPath>()
        .map(|p| p.to_string())
        .or_else(|_| String::try_from(val))
}

fn parse_i64(val: OwnedValue) -> Result<i64> {
    i64::try_from(&val)
        .or_else(|_| i32::try_from(&val).map(Into::into))
        .context("Error downcasting i64/i32")
        .or_else(|_| {
            u64::try_from(val)
                .context("Error downcasting u64")
                .and_then(|u| u.try_into().context("Error converting u64 to i64"))
        })
}

fn parse_datetime(val: OwnedValue) -> Result<DateTime> {
    let s = String::try_from(val).context("Unable to downcast value to string")?;
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

impl TryFrom<PlayerStatus> for NowPlaying {
    type Error = Error;

    fn try_from(status: PlayerStatus) -> Result<Self> {
        trace!("Full now-playing response: {status:?}");
        let PlayerStatus {
            kind,
            bus,
            ident,
            status,
            volume,
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
            player: Player { bus, id },
            status,
            volume: volume.is_finite().then_some(volume),
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

impl MatchPlayer for NowPlaying {
    fn bus(&self) -> &str { self.player.bus.as_ref().map_or("", |s| s) }

    fn status(&self) -> PlaybackStatus { self.status }
}

struct StatusCommand<'a>(&'a PlayerOpts);
impl<'p> super::ProxyCommand<'p> for StatusCommand<'_> {
    type Output = PlayerStatus;
    type Then = PlayerStatus;

    async fn run(&self, proxy: &EmpressProxy<'p>) -> fdo::Result<Self::Output> {
        if *self.0 == PlayerOpts::default() {
            proxy.now_playing().await
        } else {
            proxy.player_status(self.0).await
        }
    }

    #[inline]
    fn then(&self, out: Self::Output) -> Result<Self::Then> { Ok(out) }
}

pub async fn run(conn: &zbus::Connection, opts: NowPlayingOpts) -> Result {
    let fmt = FormatData::prepare(&opts.format)?;
    let mut fmt = Formatter::new(&fmt, &opts)?;

    let player = opts.player.into();
    let status = super::try_run(conn, StatusCommand(&player)).await?;

    let status = status.try_into()?;
    fmt.print(&status, io::stdout().lock())?;

    if opts.watch {
        run_watch(conn, player, fmt, status).await?;
    }

    Ok(())
}

enum WatchEvent<'a> {
    TickSec,
    TickThrottled,
    Update(Option<PropertyChanged<'a, PlayerStatus>>),
    Stop(Result<(), io::Error>),
}

const MICROS_PER_SEC: i64 = 1_000_000;
const THROTTLE_MILLIS: u32 = 60;

async fn watch_tick(status: &NowPlaying) -> Option<WatchEvent<'static>> {
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
        Some(WatchEvent::TickSec)
    } else {
        tokio::time::sleep(Duration::from_millis(THROTTLE_MILLIS.into())).await;
        Some(WatchEvent::TickThrottled)
    }
}

async fn run_watch(
    conn: &zbus::Connection,
    player: server::PlayerOpts,
    mut fmt: Formatter<'_>,
    mut status: NowPlaying,
) -> Result<()> {
    let mut position = status.capture_position();

    let player = player.build().context("Invalid player filter options")?;
    let proxy = super::dial(conn).await?;
    let mut stream = proxy
        .run(
            Duration::from_secs(1),
            EmpressProxy::receive_now_playing_changed,
        )
        .await
        .context("Error getting now-playing status")?;

    loop {
        let event = tokio::select! {
            Some(e) = watch_tick(&status) => e,
            s = stream.next() => WatchEvent::Update(s),
            r = tokio::signal::ctrl_c() => WatchEvent::Stop(r),
        };

        match event {
            WatchEvent::TickSec => {
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
            WatchEvent::TickThrottled => status.position = position.map(|p| p.get(None)),
            WatchEvent::Update(Some(s)) => {
                let next: NowPlaying = Timeout::from(s)
                    .try_run(Duration::from_secs(1), PropertyChanged::get)
                    .await
                    .context("Error parsing property value")?
                    .try_into()?;

                if next == status || !player.is_match(&next) {
                    continue;
                }

                status = next;
                position = status.capture_position();
            },
            WatchEvent::Update(None) => break Err(anyhow::anyhow!("Empress server hung up")),
            WatchEvent::Stop(r) => break r.context("Error catching ^C"),
        }

        fmt.print(&status, io::stdout().lock())?;
    }
}

#[rustfmt::skip]
const FORMAT_PRETTY: &str = "\
    if volume != nil put '[', volume | percent, '] ' end \
\
    if status == 'Stopped' \
        put 'not playing'\
    else \
        put status | symbol, ' '\
\
        if blank([title, artists, album])\
            put player.id \
        else \
            if !blank(artists)\
                put artists | compact | join(', ') ?? 'Unknown', ' \\u{2014} '\
            end \
\
            put title ?? 'No Title'\
            if !blank(album) put ' (', album, ')' end \
        end \
\
        let ts := [position |! time, length |! time] | compact | join('/')\
        if !blank(ts) put ' [', ts, ']' end \
    end\
";

enum FormatData<'a> {
    Json,
    Pretty(Cow<'a, str>, bool),
}

impl<'a> FormatData<'a> {
    fn prepare(format: &'a NowPlayingFormat) -> Result<Self> {
        Ok(match (format.kind, &format.string, &format.file) {
            (k, None, None) => match k {
                FormatKind::Json => Self::Json,
                FormatKind::Pretty => Self::Pretty(FORMAT_PRETTY.into(), true),
            },
            (FormatKind::Pretty, Some(s), None) => Self::Pretty(s.into(), format.extended),
            (FormatKind::Pretty, None, Some(p)) => {
                let mut s = fs::read_to_string(p)
                    .with_context(|| format!("Error reading format from {p:?}"))?;

                if let Some(t) = s.strip_suffix('\n') {
                    s.truncate(t.len());
                }

                Self::Pretty(s.into(), format.extended)
            },
            _ => unreachable!(),
        })
    }
}

enum FormatterType<'a> {
    Json,
    Pretty(format::Formatter<'a>),
}

enum FormatterSep {
    Watch(char),
    Oneshot(bool),
}

struct Formatter<'a> {
    ty: FormatterType<'a>,
    sep: FormatterSep,
    last: Option<String>,
}

impl<'a> Formatter<'a> {
    fn new(data: &'a FormatData, opts: &NowPlayingOpts) -> Result<Self> {
        Ok(Self {
            ty: match data {
                FormatData::Json => FormatterType::Json,
                FormatData::Pretty(s, e) => {
                    FormatterType::Pretty(format::Formatter::compile(s, *e)?)
                },
            },
            sep: match (opts.watch, opts.zero, opts.no_lf) {
                (true, z, false) => FormatterSep::Watch(if z { '\0' } else { '\n' }),
                (false, false, n) => FormatterSep::Oneshot(!n),
                _ => unreachable!(),
            },
            last: None,
        })
    }

    fn print(&mut self, status: &NowPlaying, mut out: impl io::Write) -> Result {
        let mut s: String = match self.ty {
            FormatterType::Json => serde_json::to_string(status)?,
            FormatterType::Pretty(ref f) => f.run(status)?,
        };

        if let FormatterSep::Watch(c) = self.sep {
            s = s.replace(c, " ");
        }

        if self.last.as_ref().is_none_or(|l| *l != s) {
            write!(out, "{s}")?;

            match self.sep {
                FormatterSep::Watch(c) => write!(out, "{c}")?,
                FormatterSep::Oneshot(true) => writeln!(out)?,
                FormatterSep::Oneshot(false) => (),
            }

            out.flush()?;

            self.last = Some(s);
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::{FormatData, Formatter, NowPlaying, Player};
    use crate::{
        opts::{FormatKind, NowPlayingFormat, NowPlayingOpts, PlayerOpts},
        server::mpris::player::PlaybackStatus,
    };

    #[test]
    fn eval_pretty() {
        let opts = NowPlayingOpts {
            player: PlayerOpts::default(),
            format: NowPlayingFormat {
                string: None,
                file: None,
                extended: false,
                kind: FormatKind::Pretty,
            },
            watch: false,
            zero: false,
            no_lf: true,
        };
        let fmt = FormatData::prepare(&opts.format).unwrap();
        let mut fmt = Formatter::new(&fmt, &opts).unwrap();

        let mut v = vec![];
        fmt.print(&NowPlaying::default(), &mut v).unwrap();
        assert_eq!(std::str::from_utf8(&v).unwrap(), "not playing");

        v.clear();
        fmt.print(
            &NowPlaying {
                status: PlaybackStatus::Playing,
                player: Player {
                    bus: Some("my-player".into()),
                    id: Some("My Player".into()),
                },
                volume: Some(0.999),
                ..NowPlaying::default()
            },
            &mut v,
        )
        .unwrap();
        assert_eq!(std::str::from_utf8(&v).unwrap(), "[99%] ▶ My Player");

        v.clear();
        fmt.print(
            &NowPlaying {
                status: PlaybackStatus::Playing,
                title: Some("test track".into()),
                artists: Some(vec!["test artist".into(), "another test".into()]),
                album: Some("test album".into()),
                position: Some(16_000_000),
                length: Some(128_000_000),
                ..NowPlaying::default()
            },
            &mut v,
        )
        .unwrap();
        assert_eq!(
            std::str::from_utf8(&v).unwrap(),
            "▶ test artist, another test — test track (test album) [0:16/2:08]"
        );
    }
}
