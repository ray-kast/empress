use std::{collections::HashMap, sync::LazyLock};

use zbus::{
    fdo,
    names::{OwnedBusName, OwnedInterfaceName},
    zvariant::{ObjectPath, OwnedObjectPath, OwnedValue},
};

pub const NAME_PREFIX: &str = "org.mpris.MediaPlayer2";
const PATH_PREFIX: &str = "/org/mpris/MediaPlayer2";

pub static BUS_NAME: LazyLock<OwnedBusName> = LazyLock::new(|| NAME_PREFIX.try_into().unwrap());
pub static OBJECT_PATH: LazyLock<OwnedObjectPath> =
    LazyLock::new(|| PATH_PREFIX.try_into().unwrap());
pub static INTERFACE: LazyLock<OwnedInterfaceName> =
    LazyLock::new(|| NAME_PREFIX.try_into().unwrap());

#[zbus::proxy(
    interface = "org.mpris.MediaPlayer2",
    default_path = "/org/mpris/MediaPlayer2"
)]
pub trait MediaPlayer {
    fn raise(&self) -> fdo::Result<()>;

    #[zbus(property)]
    fn can_raise(&self) -> fdo::Result<bool>;
    #[zbus(property)]
    fn identity(&self) -> fdo::Result<String>;
}

#[zbus::proxy(
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

    #[zbus(signal)]
    fn seeked(&self, pos: i64) -> fdo::Result<()>;

    #[zbus(property)]
    fn playback_status(&self) -> fdo::Result<player::PlaybackStatus>;
    #[zbus(property)]
    fn rate(&self) -> fdo::Result<f64>;
    #[zbus(property)]
    fn metadata(&self) -> fdo::Result<HashMap<String, OwnedValue>>;
    #[zbus(property)]
    fn volume(&self) -> fdo::Result<f64>;
    #[zbus(property)]
    fn set_volume(&self, vol: f64) -> fdo::Result<()>;
    #[zbus(property(emits_changed_signal = "false"))]
    fn position(&self) -> fdo::Result<i64>;
    #[zbus(property)]
    fn can_go_next(&self) -> fdo::Result<bool>;
    #[zbus(property)]
    fn can_go_previous(&self) -> fdo::Result<bool>;
    #[zbus(property)]
    fn can_play(&self) -> fdo::Result<bool>;
    #[zbus(property)]
    fn can_pause(&self) -> fdo::Result<bool>;
    #[zbus(property)]
    fn can_seek(&self) -> fdo::Result<bool>;
    #[zbus(property)]
    fn can_control(&self) -> fdo::Result<bool>;
}

pub mod player {
    use std::sync::LazyLock;

    use zbus::{
        names::OwnedInterfaceName,
        zvariant::{self, Error as VariantError, OwnedValue},
    };

    use super::NAME_PREFIX;

    pub static INTERFACE: LazyLock<OwnedInterfaceName> =
        LazyLock::new(|| format!("{NAME_PREFIX}.Player").try_into().unwrap());

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

    // These are all trivial, but this should help clarify a lot of ad-hoc
    // logic full of Boolean connectives elsewhere
    impl PlaybackStatus {
        pub const fn is_playing(self) -> bool { matches!(self, Self::Playing) }

        pub const fn can_play(self) -> bool { matches!(self, Self::Paused) }

        pub const fn can_pause(self) -> bool { matches!(self, Self::Playing) }

        pub const fn can_stop(self) -> bool { !matches!(self, Self::Stopped) }
    }

    impl<'a> TryFrom<zvariant::Value<'a>> for PlaybackStatus {
        type Error = zvariant::Error;

        fn try_from(value: zvariant::Value<'a>) -> Result<Self, Self::Error> {
            let s: zvariant::Str = value.try_into()?;

            s.parse().map_err(|e: strum::ParseError| {
                zvariant::Error::InputOutput(std::sync::Arc::new(std::io::Error::other(e)))
            })
        }
    }

    impl From<PlaybackStatus> for zvariant::Value<'_> {
        fn from(value: PlaybackStatus) -> Self { value.to_string().into() }
    }

    impl From<PlaybackStatus> for String {
        fn from(s: PlaybackStatus) -> Self { s.to_string() }
    }

    impl TryFrom<String> for PlaybackStatus {
        type Error = <Self as std::str::FromStr>::Err;

        fn try_from(value: String) -> Result<Self, Self::Error> { value.parse() }
    }

    impl TryFrom<OwnedValue> for PlaybackStatus {
        type Error = VariantError;

        fn try_from(value: OwnedValue) -> Result<Self, Self::Error> {
            value
                .downcast_ref::<&str>()?
                .parse()
                .map_err(|e: strum::ParseError| VariantError::Message(e.to_string()))
        }
    }
}

pub mod track_list {
    use std::sync::LazyLock;

    use super::{OwnedObjectPath, PATH_PREFIX};

    /// Type: `ObjectPath`
    pub const ATTR_TRACK_ID: &str = "mpris:trackid";
    /// Type: `i64`
    pub const ATTR_LENGTH: &str = "mpris:length";
    /// Type: `Url`
    pub const ATTR_ART_URL: &str = "mpris:artUrl";

    /// Type: `String`
    pub const ATTR_ALBUM: &str = "xesam:album";
    /// Type: `Vec<String>`
    pub const ATTR_ALBUM_ARTISTS: &str = "xesam:albumArtist";
    /// Type: `Vec<String>`
    pub const ATTR_ARTISTS: &str = "xesam:artist";
    /// Type: `String`
    pub const ATTR_LYRICS: &str = "xesam:asText";
    /// Type: `{integer}`
    pub const ATTR_BPM: &str = "xesam:audioBPM";
    /// Type: `f64` in [0.0, 1.0]
    pub const ATTR_AUTO_RATING: &str = "xesam:autoRating";
    /// Type: `Vec<String>`
    pub const ATTR_COMMENTS: &str = "xesam:comment";
    /// Type: `Vec<String>`
    pub const ATTR_COMPOSERS: &str = "xesam:composer";
    /// Type: `DateTime`
    pub const ATTR_DATE_CREATED: &str = "xesam:contentCreated";
    /// Type: `{integer}`
    pub const ATTR_DISC_NUM: &str = "xesam:discNumber";
    /// Type: `DateTime`
    pub const ATTR_DATE_FIRST_PLAYED: &str = "xesam:firstUsed";
    /// Type: `Vec<String>`
    pub const ATTR_GENRES: &str = "xesam:genre";
    /// Type: `DateTime`
    pub const ATTR_DATE_LAST_PLAYED: &str = "xesam:lastUsed";
    /// Type: `Vec<String>`
    pub const ATTR_LYRICISTS: &str = "xesam:lyricist";
    /// Type: `String`
    pub const ATTR_TITLE: &str = "xesam:title";
    /// Type: `{integer}`
    pub const ATTR_TRACK_NUM: &str = "xesam:trackNumber";
    /// Type: `Url`
    pub const ATTR_URL: &str = "xesam:url";
    /// Type: `{integer}`
    pub const ATTR_PLAY_COUNT: &str = "xesam:useCount";
    /// Type: `f64` in [0.0, 1.0]
    pub const ATTR_USER_RATING: &str = "xesam:userRating";

    pub static NO_TRACK: LazyLock<OwnedObjectPath> = LazyLock::new(|| {
        format!("{PATH_PREFIX}/TrackList/NoTrack")
            .try_into()
            .unwrap()
    });
}
