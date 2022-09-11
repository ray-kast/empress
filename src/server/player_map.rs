use std::{
    collections::{BTreeSet, HashMap, HashSet},
    time::Instant,
};

use log::{debug, trace, warn};
use zbus::{names::BusName, Connection};

use super::{mpris::player::PlaybackStatus, Player};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct PlayerKey {
    status: PlaybackStatus,
    last_update: Instant,
    bus: BusName<'static>,
}

trait GetKey {
    fn get_key(&self) -> PlayerKey;
}

impl GetKey for Player {
    fn get_key(&self) -> PlayerKey {
        PlayerKey {
            status: self.status(),
            last_update: self.last_update(),
            bus: self.bus().to_owned(),
        }
    }
}

#[derive(Debug)]
pub(super) struct PlayerMap {
    players: HashMap<BusName<'static>, Player>,
    keys: BTreeSet<PlayerKey>,
}

impl PlayerMap {
    pub async fn inform(
        &mut self,
        conn: &Connection,
        now: Instant,
        try_patch: bool,
        names: HashSet<BusName<'static>>,
    ) {
        let key_set: HashSet<_> = self.players.keys().cloned().collect();

        for name in key_set.difference(&names) {
            trace!("Removing player from map: {:?}", name);

            assert!(self.remove(name).is_some());
        }

        for name in names.difference(&key_set) {
            let player = match Player::new(now, name, conn).await {
                Ok(p) => p,
                Err(e) => {
                    warn!("Constructing player failed: {:?}", e);
                    continue;
                },
            };

            trace!("Adding new player to map: {:?}", player);

            assert!(self.put(player));
        }

        if try_patch {
            for name in names.intersection(&key_set) {
                let mut player = self.players.remove(name).unwrap();

                match player.refresh().await {
                    Ok(()) => (),
                    Err(e) => {
                        warn!("Refreshing player failed: {:?}", e);
                        continue;
                    },
                }

                trace!("Updating player in map: {:?}", player);
                assert!(!self.put(player));
            }
        }
    }

    pub fn new() -> Self {
        Self {
            players: HashMap::new(),
            keys: BTreeSet::new(),
        }
    }

    pub fn iter_all(&self) -> impl Iterator<Item = &Player> {
        self.keys.iter().map(|k| &self.players[&k.bus])
    }

    /// Returns all players whose status matches that of the first (i.e.
    /// highest-priority) player in the list.  This should be used when
    /// selecting a player to run an action on, to avoid unexpected behavior.
    pub fn iter_active(&self) -> impl Iterator<Item = &Player> {
        let mut prev = None;

        self.keys
            .iter()
            .take_while(move |p| {
                let ret = prev.map_or(true, |s| s == p.status);
                prev = Some(p.status);

                if !ret {
                    debug!("Stopping active-player search before {:?}", p);
                }

                ret
            })
            .map(|k| &self.players[&k.bus])
    }

    /// Always updates the map, but only returns true if a new key was inserted
    pub fn put(&mut self, player: Player) -> bool {
        use std::collections::hash_map::Entry;

        match self.players.entry(player.bus().to_owned()) {
            Entry::Vacant(v) => {
                trace!("Inserting new player into map: {:?}", player);

                self.keys.insert(player.get_key());
                v.insert(player);
                true
            },
            Entry::Occupied(o) => {
                trace!("Patching existing player in map: {:?}", player);

                let old_player = o.into_mut();
                assert!(self.keys.remove(&old_player.get_key()));
                self.keys.insert(player.get_key());
                *old_player = player;
                false
            },
        }
    }

    pub fn remove(&mut self, bus: &BusName<'static>) -> Option<Player> {
        if let Some(old_player) = self.players.remove(bus) {
            trace!("Removing player from map: {:?}", bus);

            assert!(self.keys.remove(&old_player.get_key()));

            Some(old_player)
        } else {
            trace!("Player to remove does not exist in map: {:?}", bus);

            None
        }
    }
}
