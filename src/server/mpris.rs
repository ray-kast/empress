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
    fn playback_status(&self) -> fdo::Result<player::PlaybackStatus>;
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
    use zbus::{
        names::OwnedInterfaceName,
        zvariant::{self, Error as VariantError, OwnedValue},
    };

    use super::{lazy_static, NAME_PREFIX};

    lazy_static! {
        pub static ref INTERFACE: OwnedInterfaceName =
            format!("{}.Player", *NAME_PREFIX).try_into().unwrap();
    }

    #[derive(
        Debug,
        Clone,
        Copy,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        Default,
        strum::EnumString,
        strum::Display,
        serde::Serialize,
        serde::Deserialize,
        zvariant::Type,
    )]
    #[serde(into = "String", try_from = "String")]
    #[zvariant(signature = "s")]
    pub enum PlaybackStatus {
        Playing,
        Paused,
        #[default]
        Stopped,
    }

    impl<'a> TryFrom<zvariant::Value<'a>> for PlaybackStatus {
        type Error = zvariant::Error;

        fn try_from(value: zvariant::Value<'a>) -> Result<Self, Self::Error> {
            let s: zvariant::Str = value.try_into()?;

            s.parse().map_err(|e: strum::ParseError| {
                zvariant::Error::InputOutput(std::sync::Arc::new(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    e,
                )))
            })
        }
    }

    impl<'a> From<PlaybackStatus> for zvariant::Value<'a> {
        fn from(value: PlaybackStatus) -> Self {
            value.to_string().into()
        }
    }

    impl From<PlaybackStatus> for String {
        fn from(s: PlaybackStatus) -> Self {
            s.to_string()
        }
    }

    impl TryFrom<String> for PlaybackStatus {
        type Error = <Self as std::str::FromStr>::Err;

        fn try_from(value: String) -> Result<Self, Self::Error> {
            value.parse()
        }
    }

    impl TryFrom<OwnedValue> for PlaybackStatus {
        type Error = VariantError;

        fn try_from(value: OwnedValue) -> Result<Self, Self::Error> {
            value
                .downcast_ref::<str>()
                .ok_or(VariantError::IncorrectType)?
                .parse()
                .map_err(|e: strum::ParseError| VariantError::Message(e.to_string()))
        }
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
