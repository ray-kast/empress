#![allow(clippy::needless_borrow)] // Clippy bug

use lazy_static::lazy_static;
use zbus::{
    names::{OwnedBusName, OwnedInterfaceName, OwnedMemberName},
    zvariant::OwnedObjectPath,
};

lazy_static! {
    pub static ref NAME_PREFIX: String = "org.mpris.MediaPlayer2".into();
    pub static ref PATH_PREFIX: String = "/org/mpris/MediaPlayer2".into();
    pub static ref BUS_NAME: OwnedBusName = NAME_PREFIX.as_str().try_into().unwrap();
    pub static ref ENTRY_PATH: OwnedObjectPath = PATH_PREFIX.as_str().try_into().unwrap();
}

pub mod root {
    use super::{lazy_static, OwnedInterfaceName, OwnedMemberName, NAME_PREFIX};

    lazy_static! {
        pub static ref INTERFACE: OwnedInterfaceName = NAME_PREFIX.clone().try_into().unwrap();
        pub static ref IDENTITY: OwnedMemberName = "Identity".try_into().unwrap();
    }
}

pub mod player {
    use super::{lazy_static, OwnedInterfaceName, OwnedMemberName, NAME_PREFIX};

    lazy_static! {
        pub static ref INTERFACE: OwnedInterfaceName =
            format!("{}.Player", *NAME_PREFIX).try_into().unwrap();
        pub static ref NEXT: OwnedMemberName = "Next".try_into().unwrap();
        pub static ref PREVIOUS: OwnedMemberName = "Previous".try_into().unwrap();
        pub static ref PAUSE: OwnedMemberName = "Pause".try_into().unwrap();
        pub static ref STOP: OwnedMemberName = "Stop".try_into().unwrap();
        pub static ref PLAY: OwnedMemberName = "Play".try_into().unwrap();
        pub static ref SET_POSITION: OwnedMemberName = "SetPosition".try_into().unwrap();
        pub static ref PLAYBACK_STATUS: OwnedMemberName = "PlaybackStatus".try_into().unwrap();
        pub static ref METADATA: OwnedMemberName = "Metadata".try_into().unwrap();
        pub static ref VOLUME: OwnedMemberName = "Volume".try_into().unwrap();
        pub static ref POSITION: OwnedMemberName = "Position".try_into().unwrap();
        pub static ref CAN_GO_NEXT: OwnedMemberName = "CanGoNext".try_into().unwrap();
        pub static ref CAN_GO_PREVIOUS: OwnedMemberName = "CanGoPrevious".try_into().unwrap();
        pub static ref CAN_PLAY: OwnedMemberName = "CanPlay".try_into().unwrap();
        pub static ref CAN_PAUSE: OwnedMemberName = "CanPause".try_into().unwrap();
        pub static ref CAN_SEEK: OwnedMemberName = "CanSeek".try_into().unwrap();
        pub static ref CAN_CONTROL: OwnedMemberName = "CanControl".try_into().unwrap();
    }

    #[derive(
        Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, strum::EnumString, strum::Display,
    )]
    pub enum PlaybackStatus {
        Playing,
        Paused,
        Stopped,
    }
}

pub mod track_list {
    use super::{lazy_static, OwnedObjectPath};

    pub const ATTR_TRACKID: &str = "mpris:trackid";
    pub const ATTR_LENGTH: &str = "mpris:length";
    pub const ATTR_TITLE: &str = "xesam:title";
    pub const ATTR_ARTIST: &str = "xesam:artist";
    pub const ATTR_ALBUM: &str = "xesam:album";

    lazy_static! {
        pub static ref PATH_PREFIX: String = format!("{}/TrackList", *super::PATH_PREFIX);
        pub static ref NO_TRACK: OwnedObjectPath =
            format!("{}/NoTrack", *PATH_PREFIX).try_into().unwrap();
    }
}
