#![allow(clippy::needless_borrow)] // Clippy bug

use dbus::{
    strings::{BusName, Interface, Member},
    Path,
};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref NAME_PREFIX: String = "org.mpris.MediaPlayer2".into();
    pub static ref PATH_PREFIX: String = "/org/mpris/MediaPlayer2".into();
    pub static ref BUS_NAME: BusName<'static> = (&*NAME_PREFIX).into();
    pub static ref ENTRY_PATH: Path<'static> = (&*PATH_PREFIX).into();
}

pub mod root {
    use super::{lazy_static, Interface, Member, NAME_PREFIX};

    lazy_static! {
        pub static ref INTERFACE: Interface<'static> = NAME_PREFIX.clone().into();
        pub static ref IDENTITY: Member<'static> = "Identity".into();
    }
}

pub mod player {
    use super::{lazy_static, Interface, Member, NAME_PREFIX};
    use crate::Result;

    lazy_static! {
        pub static ref INTERFACE: Interface<'static> = format!("{}.Player", *NAME_PREFIX).into();
        pub static ref NEXT: Member<'static> = "Next".into();
        pub static ref PREVIOUS: Member<'static> = "Previous".into();
        pub static ref PAUSE: Member<'static> = "Pause".into();
        pub static ref STOP: Member<'static> = "Stop".into();
        pub static ref PLAY: Member<'static> = "Play".into();
        pub static ref SET_POSITION: Member<'static> = "SetPosition".into();
        pub static ref PLAYBACK_STATUS: Member<'static> = "PlaybackStatus".into();
        pub static ref METADATA: Member<'static> = "Metadata".into();
        pub static ref VOLUME: Member<'static> = "Volume".into();
        pub static ref POSITION: Member<'static> = "Position".into();
        pub static ref CAN_GO_NEXT: Member<'static> = "CanGoNext".into();
        pub static ref CAN_GO_PREVIOUS: Member<'static> = "CanGoPrevious".into();
        pub static ref CAN_PLAY: Member<'static> = "CanPlay".into();
        pub static ref CAN_PAUSE: Member<'static> = "CanPause".into();
        pub static ref CAN_SEEK: Member<'static> = "CanSeek".into();
        pub static ref CAN_CONTROL: Member<'static> = "CanControl".into();
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum PlaybackStatus {
        Playing,
        Paused,
        Stopped,
    }

    impl std::fmt::Display for PlaybackStatus {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(match self {
                Self::Playing => "Playing",
                Self::Paused => "Paused",
                Self::Stopped => "Stopped",
            })
        }
    }

    impl std::str::FromStr for PlaybackStatus {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self> {
            Ok(match s {
                "Playing" => Self::Playing,
                "Paused" => Self::Paused,
                "Stopped" => Self::Stopped,
                s => return Err(anyhow::anyhow!("unexpected playback status {:?}", s)),
            })
        }
    }
}

pub mod track_list {
    use super::{lazy_static, Path};

    pub const ATTR_TRACKID: &str = "mpris:trackid";
    pub const ATTR_TITLE: &str = "xesam:title";
    pub const ATTR_ARTIST: &str = "xesam:artist";
    pub const ATTR_ALBUM: &str = "xesam:album";

    lazy_static! {
        pub static ref PATH_PREFIX: String = format!("{}/TrackList", *super::PATH_PREFIX);
        pub static ref NO_TRACK: Path<'static> = format!("{}/NoTrack", *PATH_PREFIX).into();
    }
}
