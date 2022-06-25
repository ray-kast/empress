use std::time::Instant;

use zbus::names::OwnedBusName;

use crate::{interface::PlaybackStatus, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct OrdBusName(pub OwnedBusName);

impl Ord for OrdBusName {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering { self.0.as_str().cmp(rhs.0.as_str()) }
}

impl PartialOrd for OrdBusName {
    #[inline]
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(rhs)) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Player {
    pub status: PlaybackStatus,
    pub last_update: Instant,
    pub bus: OrdBusName,
}

impl Player {
    pub async fn new(now: Instant, name: OwnedBusName, conn: &zbus::Connection) -> Result<Self> {
        todo!()
    }
}
