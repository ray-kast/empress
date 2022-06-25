use std::sync::Arc;

use lazy_static::lazy_static;
pub use zbus::fdo::Error;
use zbus::{
    dbus_interface, dbus_proxy, names::OwnedWellKnownName, zvariant, zvariant::OwnedObjectPath,
    Connection,
};

use crate::server::Handler;

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

pub type Result<T = ()> = zbus::fdo::Result<T>;
type ZResult<T = ()> = zbus::Result<T>;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
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
    fn list_players(&self) -> ZResult<Vec<(String, PlaybackStatus)>>;

    fn next(&self, search: PlayerSearchOpts) -> ZResult;

    fn now_playing(&self, search: PlayerSearchOpts) -> ZResult<NowPlayingResponse>;

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

pub struct Daemon(pub Arc<Handler>);

#[dbus_interface(interface = "net.ryan_s.Empress1.Daemon")]
impl Daemon {
    async fn list_players(
        &self,
        #[zbus(connection)] conn: &Connection,
    ) -> Result<Vec<(String, PlaybackStatus)>> {
        self.0.handle(Handler::list_players, conn, ()).await
    }

    async fn next(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
    ) -> Result<()> {
        self.0.handle(Handler::next, conn, search).await
    }

    async fn now_playing(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
    ) -> Result<NowPlayingResponse> {
        self.0.handle(Handler::now_playing, conn, search).await
    }

    async fn pause(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
    ) -> Result<()> {
        self.0.handle(Handler::pause, conn, search).await
    }

    async fn play(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
    ) -> Result<()> {
        self.0.handle(Handler::play, conn, search).await
    }

    async fn play_pause(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
    ) -> Result<()> {
        self.0.handle(Handler::play_pause, conn, search).await
    }

    async fn previous(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
    ) -> Result<()> {
        self.0.handle(Handler::previous, conn, search).await
    }

    async fn seek_absolute(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
        to: f64,
    ) -> Result<f64> {
        self.0
            .handle(Handler::seek_absolute, conn, (search, to))
            .await
    }

    async fn seek_relative(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
        by: f64,
    ) -> Result<f64> {
        self.0
            .handle(Handler::seek_relative, conn, (search, by))
            .await
    }

    async fn stop(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
    ) -> Result<()> {
        self.0.handle(Handler::stop, conn, search).await
    }

    async fn switch_current(
        &self,
        #[zbus(connection)] conn: &Connection,
        to: String,
        switch_playing: bool,
    ) -> Result<()> {
        self.0
            .handle(Handler::switch_current, conn, (to, switch_playing))
            .await
    }

    async fn vol_absolute(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
        to: f64,
    ) -> Result<f64> {
        self.0
            .handle(Handler::vol_absolute, conn, (search, to))
            .await
    }

    async fn vol_relative(
        &self,
        #[zbus(connection)] conn: &Connection,
        search: PlayerSearchOpts,
        by: f64,
    ) -> Result<f64> {
        self.0
            .handle(Handler::vol_relative, conn, (search, by))
            .await
    }
}
