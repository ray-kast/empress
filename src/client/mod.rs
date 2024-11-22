use std::{future::Future, io, io::IsTerminal, time::Duration};

use anyhow::{Context, Error};
use futures_util::StreamExt;
use jiff::civil::DateTime;
use log::{info, trace, warn};
use serde::Serialize;
use zbus::{
    connection,
    zvariant::{ObjectPath, OwnedValue},
};

use self::proxy::EmpressProxy;
use crate::{
    format,
    server::{
        self, mpris, mpris::player::PlaybackStatus, MatchPlayer, PlayerStatus, PlayerStatusKind,
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
    fn bus(&self) -> &str {
        self.player.bus.as_ref().map_or("", |s| s)
    }

    fn status(&self) -> PlaybackStatus {
        self.status
    }
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
        } => now_playing(proxy, player, format, watch).await?,
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

async fn now_playing(
    proxy: Timeout<EmpressProxy<'_>>,
    player: PlayerOpts,
    format: Option<String>,
    watch: bool,
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

    let format = format.as_deref();
    let mut last = print_now_playing(status, format, |_| true)?.1;

    if watch {
        println!();

        let player = player.build().context("Invalid player filter options")?;
        let mut stream = proxy
            .run(
                Duration::from_secs(1),
                EmpressProxy::receive_now_playing_changed,
            )
            .await
            .context("Error getting now-playing status")?;

        while let Some(s) = stream.next().await {
            let val = s.get().await.context("Error parsing property value")?;

            trace!("Full now-playing response: {val:?}");

            let (skip, val) = print_now_playing(val, format, |v| *v != last && player.is_match(v))?;
            if skip {
                continue;
            }

            println!();

            last = val;
        }
    } else {
        courtesy_line!();
    }

    Ok(())
}

fn print_now_playing(
    resp: PlayerStatus,
    format: Option<&str>,
    f: impl FnOnce(&NowPlayingResult) -> bool,
) -> Result<(bool, NowPlayingResult)> {
    trace!("Full now-playing response: {resp:?}");

    let resp: NowPlayingResult = resp.try_into()?;

    if !f(&resp) {
        return Ok((true, resp));
    }

    if let Some(format) = format {
        print!("{}", format::eval(format, &resp)?);
    } else {
        serde_json::to_writer(io::stdout(), &resp)?;
    }

    Ok((false, resp))
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
    }
}
