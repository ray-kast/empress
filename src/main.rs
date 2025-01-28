#![deny(
    clippy::disallowed_methods,
    clippy::suspicious,
    clippy::style,
    clippy::clone_on_ref_ptr,
    rustdoc::broken_intra_doc_links,
    missing_debug_implementations,
    missing_copy_implementations
)]
#![warn(clippy::pedantic, clippy::cargo, missing_docs)]

//! Binary crate for `empress`.  See [the
//! README](https://github.com/ray-kast/empress/blob/master/README.md) for more
//! details.

use std::{
    fmt::{self, Display, Formatter},
    io::IsTerminal,
    path::PathBuf,
    sync::LazyLock,
};

use anyhow::{anyhow, Context, Error};
use clap::Parser;
use log::{error, LevelFilter};
use tokio::runtime::Builder as RtBuilder;
use zbus::{names::OwnedWellKnownName, zvariant::OwnedObjectPath};

mod client;
mod format;
mod server;
mod timeout;

type Result<T = (), E = Error> = std::result::Result<T, E>;

#[cfg(debug_assertions)]
const SERVER_NAME_STR: &str = "club.bnuy.debug.Empress";

#[cfg(not(debug_assertions))]
const SERVER_NAME_STR: &str = "club.bnuy.Empress";

#[cfg(test)]
static INTERFACE_ID: LazyLock<String> =
    LazyLock::new(|| SERVER_NAME_STR.replace(".debug", "") + ".Daemon");

static SERVER_NAME: LazyLock<OwnedWellKnownName> =
    LazyLock::new(|| SERVER_NAME_STR.try_into().unwrap());
static SERVER_PATH: LazyLock<OwnedObjectPath> =
    LazyLock::new(|| "/club/bnuy/Empress/Daemon".try_into().unwrap());

#[cfg(test)]
#[test]
fn assert_path() {
    assert_eq!(
        SERVER_PATH
            .as_str()
            .strip_prefix('/')
            .unwrap()
            .replace('/', "."),
        *INTERFACE_ID
    );
}

#[derive(Parser)]
#[command(author, about, version, max_term_width = 80)]
struct Opts {
    #[command(flatten)]
    log: LogOpts,

    #[command(subcommand)]
    cmd: Command,
}

#[derive(Default, clap::Args)]
struct LogOpts {
    /// Disable info logs (enabled by default if stderr is not a TTY)
    #[arg(short, long, global = true)]
    quiet: bool,

    /// Enable info logs, even if stderr is not a TTY
    #[arg(long, conflicts_with("quiet"), global = true)]
    no_quiet: bool,

    /// Output extra information to the console - specify multiple times to
    /// increase the log level further (maximum is TRACE)
    #[arg(
        short,
        long,
        global = true,
        action(clap::ArgAction::Count),
        conflicts_with("quiet"),
        conflicts_with("no_quiet")
    )]
    verbose: u8,
}

impl LogOpts {
    fn init(self) -> Result {
        const VERBOSITY: [LevelFilter; 3] =
            [LevelFilter::Info, LevelFilter::Debug, LevelFilter::Trace];
        #[cfg(debug_assertions)]
        const DEFAULT_V: u8 = 1;
        #[cfg(not(debug_assertions))]
        const DEFAULT_V: u8 = 0;

        let Self {
            quiet,
            no_quiet,
            verbose,
        } = self;

        env_logger::builder()
            .parse_default_env()
            .filter_level(
                if quiet || !(no_quiet || verbose != 0 || std::io::stderr().is_terminal()) {
                    LevelFilter::Warn
                } else {
                    VERBOSITY[usize::from(DEFAULT_V.saturating_add(verbose))
                        .min(VERBOSITY.len().saturating_sub(1))]
                },
            )
            .try_init()
            .map_err(Into::into)
    }
}

#[derive(clap::Subcommand)]
enum Command {
    /// Launch a D-Bus service abstracting MPRIS players
    Server,
    #[command(flatten)]
    Client(ClientCommand),
}

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum FormatKind {
    /// Output the raw data formatted as JSON
    Json,
    /// Output the data in a human-readable format
    Pretty,
}

#[derive(Debug, Clone, clap::Args)]
struct NowPlayingFormat {
    /// Instead of outputting in a preset format, specify a format string to be
    /// evaluated to pretty-print the command output - see the full help for
    /// this command for a syntax reference
    #[arg(
        short = 'f',
        long = "format",
        conflicts_with("file"),
        conflicts_with("kind")
    )]
    string: Option<String>,

    /// Specify a path to a file containing a format string, to be evaluated
    /// in a similar fashion to the -f flag
    #[arg(
        short = 'F',
        long = "format-from",
        conflicts_with("string"),
        conflicts_with("kind")
    )]
    file: Option<PathBuf>,

    /// Specify the type of the output (e.g. JSON) - cannot be used with
    /// explicitly-specified format strings
    #[arg(
        short = 'o',
        long = "output",
        conflicts_with("string"),
        conflicts_with("file")
    )]
    kind: Option<FormatKind>,
}

#[derive(Debug, Clone, clap::Subcommand)]
enum ClientCommand {
    /// Scan for any player updates the daemon missed
    Scan,
    /// List the players currently tracked by the daemon
    ListPlayers,
    /// Print information about the current track
    #[command(long_about = include_str!("../etc/now_playing_long.txt"))]
    NowPlaying {
        #[command(flatten)]
        player: PlayerOpts,

        #[command(flatten)]
        format: NowPlayingFormat,

        /// Continue watching for changes to playback status and printing
        /// updates
        #[arg(short, long, conflicts_with("PlayerOpts"))]
        watch: bool,

        /// Separate outputs with a null byte rather than a newline.  Only
        /// usable with the -w flag.
        #[arg(short = '0', long, requires("watch"))]
        zero: bool,
    },
    /// Focus a player
    Raise(PlayerOpts),
    /// Skip one track forwards
    Next(PlayerOpts),
    /// Skip one track backwards
    Previous(PlayerOpts),
    /// Pause a currently-playing player
    Pause(PlayerOpts),
    /// Like pause if a player is playing, otherwise like play
    PlayPause(PlayerOpts),
    /// Stop a currently-playing player
    Stop(PlayerOpts),
    /// Play a currently-paused player
    Play(PlayerOpts),
    /// Seek to a position on a player
    Seek {
        #[command(flatten)]
        player: PlayerOpts,

        /// The position to seek to, either absolute (e.g. 5) or relative (e.g.
        /// 5+ or 5-)
        to: Offset,
    },
    /// Get or set the volume on a player
    Volume {
        #[command(flatten)]
        player: PlayerOpts,

        /// The volume as a number between 0.0 and 1.0, either absolute (e.g.
        /// 0.5) or relative (e.g. 0.1+ or 0.1-).  If no value is given
        /// the current volume is simply printed instead.
        #[arg(default_value_t = Offset::Relative(0.0))]
        vol: Offset,
    },
    /// Bump the priority of a specific player
    ///
    /// Note that if --no-play is passed, any players with a status of Playing
    /// will still hold priority over the selected player.
    SwitchCurrent {
        /// The player ID to switch to.  For a list of valid players see the
        /// list-players subcommand.
        to: String,

        /// By default switch-current will pause any currently running players
        /// and play the selected player.  Pass this flag to disable
        /// this behavior.
        #[arg(short, long)]
        no_play: bool,
    },
}

/// The current status of a player
#[derive(Debug, Clone, Copy, clap::ValueEnum)]
pub enum PlaybackStatus {
    /// Indicates a player actively playing media
    Playing,
    /// Indicates a player with media loaded but not playing
    Paused,
    /// Indicates a fully-stopped player
    Stopped,
}

impl From<PlaybackStatus> for server::mpris::player::PlaybackStatus {
    fn from(value: PlaybackStatus) -> Self {
        use server::mpris::player;

        match value {
            PlaybackStatus::Playing => player::PlaybackStatus::Playing,
            PlaybackStatus::Paused => player::PlaybackStatus::Paused,
            PlaybackStatus::Stopped => player::PlaybackStatus::Stopped,
        }
    }
}

/// Options for filtering the search set of players for the daemon
#[derive(Debug, Clone, clap::Args)]
pub struct PlayerOpts {
    /// Select players whose bus names match the given regular expression
    #[arg(short, long, conflicts_with("ibus"))]
    bus: Option<String>,

    /// Select players whose bus names match the given regular expression, ignoring case
    #[arg(short, long, conflicts_with("bus"))]
    ibus: Option<String>,

    /// Select players whose state matches one of the given states
    #[arg(long, use_value_delimiter(true))]
    state: Vec<PlaybackStatus>,
}

impl From<PlayerOpts> for server::PlayerOpts {
    fn from(value: PlayerOpts) -> Self {
        let PlayerOpts { bus, ibus, state } = value;

        let (bus, ignore_bus_case) = match (bus, ibus) {
            (None, None) => (String::new(), false),
            (Some(i), None) => (i, false),
            (None, Some(i)) => (i, true),
            (Some(_), Some(_)) => unreachable!(),
        };

        server::PlayerOpts {
            bus,
            ignore_bus_case,
            states: state.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Offset {
    Relative(f64),
    Absolute(f64),
}

impl Offset {
    fn offset(&self) -> f64 {
        *match self {
            Self::Relative(o) | Self::Absolute(o) => o,
        }
    }
}

impl Display for Offset {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let offs = self.offset();
        assert!(offs.is_sign_positive());
        write!(f, "{offs}")?;

        if matches!(self, Self::Relative(..)) {
            f.write_str(if offs.is_sign_negative() { "-" } else { "+" })?;
        }

        Ok(())
    }
}

impl ::std::str::FromStr for Offset {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use regex::Regex;

        static SEEK_PATTERN: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"(\d*(?:\.\d+)?)(\+|-)?").unwrap());

        if let Some(caps) = SEEK_PATTERN.captures(s) {
            let time: f64 = caps[1].parse().context("Invalid number for offset")?;

            Ok(match caps.get(2).map(|c| c.as_str()) {
                Some("-") => Offset::Relative(-time),
                Some("+") => Offset::Relative(time),
                Some(_) => unreachable!(),
                None => Offset::Absolute(time),
            })
        } else {
            Err(anyhow!("Invalid format for seek position"))
        }
    }
}

fn main() {
    let result = run();

    match result {
        Ok(()) => (),
        Err(e) => {
            LogOpts::default().init().ok();

            error!("{e:?}");
        },
    }
}

fn run() -> Result {
    let rt = RtBuilder::new_current_thread()
        .enable_all()
        .build()
        .context("Error starting async runtime")?;

    let Opts { log, cmd } = Opts::parse();
    log.init().unwrap();

    match cmd {
        Command::Server => rt.block_on(server::run()),
        Command::Client(id) => rt.block_on(client::run(id)),
    }
}
