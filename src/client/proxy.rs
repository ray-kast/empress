use zbus::fdo;

use crate::{
    server::{PlayerList, PlayerStatus},
    PlayerOpts,
};

#[zbus::dbus_proxy(
    interface = "net.ryan_s.Empress2.Daemon",
    default_service = "net.ryan_s.Empress2",
    default_path = "/net/ryan_s/Empress2/Daemon"
)]
trait Empress {
    fn scan(&self) -> fdo::Result<Vec<String>>;

    fn list_players(&self) -> fdo::Result<PlayerList>;

    fn player_status(&self, opts: &PlayerOpts) -> fdo::Result<PlayerStatus>;

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

#[cfg(test)]
mod tests {
    use zbus::ProxyDefault;

    use super::EmpressProxy;

    #[test]
    fn test_interface() {
        assert_eq!(EmpressProxy::INTERFACE, crate::INTERFACE_NAME.as_str());
    }

    #[test]
    fn test_destination() {
        assert_eq!(
            EmpressProxy::DESTINATION,
            crate::SERVER_NAME.replace(".debug.", ".")
        );
    }

    #[test]
    fn test_path() {
        assert_eq!(
            EmpressProxy::PATH,
            crate::SERVER_PATH.replace("/debug/", "/")
        );
    }
}
