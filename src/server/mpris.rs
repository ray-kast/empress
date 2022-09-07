use std::collections::HashMap;

use lazy_static::lazy_static;
use zbus::{
    dbus_proxy, fdo,
    names::OwnedBusName,
    zvariant::{ObjectPath, OwnedObjectPath, OwnedValue},
};

lazy_static! {
    pub static ref NAME_PREFIX: String = "org.mpris.MediaPlayer2".into();
    pub static ref BUS_NAME: OwnedBusName = NAME_PREFIX.as_str().try_into().unwrap();
}

#[dbus_proxy(
    interface = "org.mpris.MediaPlayer2",
    default_path = "/org/mpris/MediaPlayer2"
)]
pub trait MediaPlayer {
    #[dbus_proxy(property)]
    fn identity(&self) -> fdo::Result<String>;
}

#[dbus_proxy(
    interface = "org.mpris.MediaPlayer2.Player",
    default_path = "/org/mpris/MediaPlayer2"
)]
pub trait Player {
    fn next(&self) -> fdo::Result<()>;
    fn previous(&self) -> fdo::Result<()>;
    fn pause(&self) -> fdo::Result<()>;
    fn play_pause(&self) -> fdo::Result<()>;
    fn stop(&self) -> fdo::Result<()>;
    fn play(&self) -> fdo::Result<()>;
    fn set_position(&self, track: ObjectPath<'_>, pos: i64) -> fdo::Result<()>;

    #[dbus_proxy(property)]
    fn playback_status(&self) -> fdo::Result<String>;
    #[dbus_proxy(property)]
    fn metadata(&self) -> fdo::Result<HashMap<String, OwnedValue>>;
    #[dbus_proxy(property)]
    fn volume(&self) -> fdo::Result<f64>;
    #[dbus_proxy(property)]
    fn set_volume(&self, vol: f64) -> fdo::Result<()>;
    #[dbus_proxy(property)]
    fn position(&self) -> fdo::Result<i64>;
    #[dbus_proxy(property)]
    fn can_go_next(&self) -> fdo::Result<bool>;
    #[dbus_proxy(property)]
    fn can_go_previous(&self) -> fdo::Result<bool>;
    #[dbus_proxy(property)]
    fn can_play(&self) -> fdo::Result<bool>;
    #[dbus_proxy(property)]
    fn can_pause(&self) -> fdo::Result<bool>;
    #[dbus_proxy(property)]
    fn can_seek(&self) -> fdo::Result<bool>;
    #[dbus_proxy(property)]
    fn can_control(&self) -> fdo::Result<bool>;
}

pub mod player {
    use zbus::names::OwnedInterfaceName;

    use super::{lazy_static, NAME_PREFIX};

    lazy_static! {
        pub static ref INTERFACE: OwnedInterfaceName =
            format!("{}.Player", *NAME_PREFIX).try_into().unwrap();
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
        pub static ref NO_TRACK: OwnedObjectPath = "/org/mpris/MediaPlayer2/TrackList/NoTrack"
            .try_into()
            .unwrap();
    }
}
