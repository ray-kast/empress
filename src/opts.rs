use std::{fmt, path::PathBuf};

// TODO: add long_help descriptions to all commands

#[derive(clap::Parser)]
#[command(author, version, max_term_width = 80)]
/// A D-Bus MPRIS daemon for controlling media players.
pub struct Opts {
    #[command(flatten)]
    log: LogOpts,

    #[command(subcommand)]
    cmd: Command,
}

#[derive(Default, clap::Args)]
pub struct LogOpts {
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

#[derive(clap::Subcommand)]
pub enum Command {
    /// Launch a D-Bus service abstracting MPRIS players
    Server,
    #[command(flatten)]
    Client(ClientCommand),
}

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
pub enum FormatKind {
    /// Output the raw data formatted as JSON
    Json,
    /// Output the data in a human-readable format
    Pretty,
}

#[derive(Debug, Clone, clap::Args)]
pub struct NowPlayingFormat {
    /// Specify a format string to be evaluated to pretty-print the command
    /// output
    #[arg(
        short = 'f',
        long = "format",
        conflicts_with("file"),
        conflicts_with("kind")
    )]
    pub string: Option<String>,

    /// Specify a path to a file containing a format string, to be evaluated
    /// in a similar fashion to the -f flag
    #[arg(
        short = 'F',
        long = "format-from",
        conflicts_with("string"),
        conflicts_with("kind")
    )]
    pub file: Option<PathBuf>,

    /// Use the extended format string syntax when parsing the given format
    /// string
    #[arg(short = 'e', long = "extended", conflicts_with("kind"))]
    pub extended: bool,

    /// Specify a preset output format
    #[arg(
        short = 'o',
        long = "output",
        default_value = "pretty",
        conflicts_with("string"),
        conflicts_with("file")
    )]
    pub kind: FormatKind,
}

#[derive(Debug, Clone, clap::Args)]
pub struct NowPlayingOpts {
    #[command(flatten)]
    pub player: PlayerOpts,

    #[command(flatten)]
    pub format: NowPlayingFormat,

    /// Continue watching for changes to playback status and printing
    /// updates
    #[arg(short, long, conflicts_with("PlayerOpts"))]
    pub watch: bool,

    /// Separate outputs with a null byte rather than a newline - only
    /// usable in watch mode
    #[arg(short = '0', long, requires("watch"))]
    pub zero: bool,

    /// Do not output a trailing newline - not usable in watch mode
    #[arg(short = 'n', long, conflicts_with("watch"))]
    pub no_lf: bool,
}

#[derive(Debug, Clone, clap::Subcommand)]
pub enum ClientCommand {
    /// Scan for any player updates the daemon missed
    Scan,
    /// List the players currently tracked by the daemon
    ListPlayers,
    /// Print information about the current track
    NowPlaying(NowPlayingOpts),
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

#[derive(Debug, Clone, Copy)]
pub enum Offset {
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

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let offs = self.offset();
        assert!(offs.is_sign_positive());
        write!(f, "{offs}")?;

        if matches!(self, Self::Relative(..)) {
            f.write_str(if offs.is_sign_negative() { "-" } else { "+" })?;
        }

        Ok(())
    }
}
