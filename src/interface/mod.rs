use lazy_static::lazy_static;
use zbus::{
    dbus_interface, dbus_proxy, names::OwnedWellKnownName, zvariant, zvariant::OwnedObjectPath,
};

pub mod mpris;

pub mod metadata {
    pub const PLAYER_BUS: &str = "empress:playerBus";
    pub const PLAYER_IDENTITY: &str = "empress:playerIdentity";
    pub const POSITION: &str = "empress:position";
}

#[cfg(debug_assertions)]
lazy_static! {
    static ref NAME_PREFIX: String = format!("net.ryan_s.debug.{}", *API_IDENT);
    static ref PATH_PREFIX: String = format!("/net/ryan_s/debug/{}", *API_IDENT);
}

#[cfg(not(debug_assertions))]
lazy_static! {
    static ref NAME_PREFIX: String = format!("net.ryan_s.{}", *API_IDENT);
    static ref PATH_PREFIX: String = format!("/net/ryan_s/{}", *API_IDENT);
}

lazy_static! {
    static ref API_IDENT: String = format!("Empress{}", env!("CARGO_PKG_VERSION_MAJOR"));
    pub static ref SERVER_NAME: OwnedWellKnownName = NAME_PREFIX.clone().try_into().unwrap();
    pub static ref SERVER_PATH: OwnedObjectPath =
        format!("{}/Daemon", *PATH_PREFIX).try_into().unwrap();
}

type ZResult<T = ()> = zbus::Result<T>;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    serde::Serialize,
    serde::Deserialize,
    strum::Display,
    zvariant::Type,
)]
pub enum PlaybackStatus {
    Playing,
    Paused,
    Stopped,
}

#[derive(Debug, zvariant::SerializeDict, zvariant::DeserializeDict, zvariant::Type)]
#[zvariant(signature = "dict")]
pub struct PlayerSearchOpts {}

// TODO: could this be a serializable struct?
pub type NowPlayingMeta = std::collections::HashMap<String, zvariant::OwnedValue>;

pub type NowPlayingResponse = (NowPlayingMeta, PlaybackStatus);

#[dbus_proxy(interface = "net.ryan_s.Empress1.Daemon")]
trait Daemon {
    fn list_players(&self) -> ZResult<Vec<(String, PlaybackStatus)>> {}

    fn next(&self, search: PlayerSearchOpts) -> ZResult;

    fn now_playing(&self, search: PlayerSearchOpts) -> ZResult<NowPlayingResponse> {}

    fn pause(&self, search: PlayerSearchOpts) -> ZResult;

    fn play(&self, search: PlayerSearchOpts) -> ZResult;

    fn play_pause(&self, search: PlayerSearchOpts) -> ZResult;

    fn previous(&self, search: PlayerSearchOpts) -> ZResult;

    fn seek_absolute(&self, search: PlayerSearchOpts, to: f64) -> ZResult<f64>;

    fn seek_relative(&self, search: PlayerSearchOpts, by: f64) -> ZResult<f64>;

    fn stop(&self, search: PlayerSearchOpts) -> ZResult;

    fn switch_current(&self, to: String, switch_playing: bool) -> ZResult;

    fn vol_absolute(&self, search: PlayerSearchOpts, to: f64) -> ZResult<f64>;

    fn vol_relative(&self, search: PlayerSearchOpts, by: f64) -> ZResult<f64>;
}

pub struct Daemon;

#[dbus_interface(interface = "net.ryan_s.Empress1.Daemon")]
impl Daemon {
    fn list_players(&self) -> Vec<(String, PlaybackStatus)> { todo!() }

    fn next(&self, search: PlayerSearchOpts) { todo!() }

    fn now_playing(&self, search: PlayerSearchOpts) -> NowPlayingResponse { todo!() }

    fn pause(&self, search: PlayerSearchOpts) { todo!() }

    fn play(&self, search: PlayerSearchOpts) { todo!() }

    fn play_pause(&self, search: PlayerSearchOpts) { todo!() }

    fn previous(&self, search: PlayerSearchOpts) { todo!() }

    fn seek_absolute(&self, search: PlayerSearchOpts, to: f64) { todo!() }

    fn seek_relative(&self, search: PlayerSearchOpts, by: f64) { todo!() }

    fn stop(&self, search: PlayerSearchOpts) { todo!() }

    fn switch_current(&self, to: String, switch_playing: bool) { todo!() }

    fn vol_absolute(&self, search: PlayerSearchOpts, to: f64) { todo!() }

    fn vol_relative(&self, search: PlayerSearchOpts, by: f64) { todo!() }
}
