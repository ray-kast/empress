use zbus::fdo;

use crate::{server::OwnedNowPlayingResponse, PlayerOpts};

#[zbus::dbus_proxy(
    interface = "net.ryan_s.Empress1.Daemon",
    default_path = "/net/ryan_s/Empress1/Daemon"
)]
trait Empress {
    fn list_players(&self) -> fdo::Result<Vec<(String, String)>>;

    fn now_playing(&self, opts: &PlayerOpts) -> fdo::Result<OwnedNowPlayingResponse>;

    fn next(&self, opts: &PlayerOpts) -> fdo::Result<()>;

    fn prev(&self, opts: &PlayerOpts) -> fdo::Result<()>;

    fn pause(&self, opts: &PlayerOpts) -> fdo::Result<()>;

    fn play_pause(&self, opts: &PlayerOpts) -> fdo::Result<()>;

    fn stop(&self, opts: &PlayerOpts) -> fdo::Result<()>;

    fn play(&self, opts: &PlayerOpts) -> fdo::Result<()>;

    fn seek_relative(&self, opts: &PlayerOpts, to: f64) -> fdo::Result<f64>;

    fn seek_absolute(&self, opts: &PlayerOpts, to: f64) -> fdo::Result<f64>;

    fn vol_relative(&self, opts: &PlayerOpts, to: f64) -> fdo::Result<f64>;

    fn vol_absolute(&self, opts: &PlayerOpts, to: f64) -> fdo::Result<f64>;

    fn switch_current(&self, to: &str, switch_playing: bool) -> fdo::Result<()>;
}
