use std::{
    collections::{BTreeSet, HashMap, HashSet},
    future::Future,
    time::Instant,
};

use futures_util::{stream::FuturesUnordered, StreamExt};
use log::{debug, trace, warn};
use tokio::sync::RwLock;
use zbus::names::BusName;

use super::{mpris::player::PlaybackStatus, Player};
use crate::Result;

// TODO: can this be zero-copy?
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
    pub async fn inform<
        P: Fn(&BusName<'static>) -> PR + Copy,
        PR: Future<Output = Result<Player>>,
    >(
        this: &RwLock<Self>,
        try_patch: bool,
        names: HashSet<BusName<'static>>,
        player: P,
    ) {
        use std::collections::hash_map::Entry;

        let key_set = this.read().await.players.keys().cloned().collect();

        if try_patch {
            // TODO: creating a new player should be avoided given the zbus
            //       proxies contain non-trivial caching logic
            let vec: Vec<_> = names
                .intersection(&key_set)
                .map(|n| async move { (n, player(n).await) })
                .collect::<FuturesUnordered<_>>()
                .collect()
                .await;

            let mut this = this.write().await;

            for (name, res) in vec {
                let player = match res {
                    Ok(p) => p,
                    Err(e) => {
                        warn!("Constructing player failed: {:?}", e);
                        continue;
                    },
                };
                let old_player = this.players.get(name).unwrap();

                if player.status() != old_player.status()
                    || player.last_update() < old_player.last_update()
                {
                    this.put(player);
                } else {
                    trace!("Skipping map update for player: {:?}", player);
                }
            }

            for name in key_set.difference(&names) {
                assert!(this.remove(name).is_some());
            }
        } else {
            let PlayerMap {
                ref mut players,
                ref mut keys,
            } = *this.write().await;

            for name in names.symmetric_difference(&key_set) {
                match players.entry(name.clone()) {
                    Entry::Vacant(v) => {
                        let player = match player(v.key()).await {
                            Ok(p) => p,
                            Err(e) => {
                                warn!("Constructing player failed: {:?}", e);
                                continue;
                            },
                        };

                        trace!("Quick-adding new player to map: {:?}", player);

                        keys.insert(player.get_key());
                        v.insert(player);
                    },
                    Entry::Occupied(o) => {
                        let (bus, player) = o.remove_entry();

                        trace!("Quick-removing player from map: {:?}", bus);

                        assert!(keys.remove(&player.get_key()));
                    },
                }
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
