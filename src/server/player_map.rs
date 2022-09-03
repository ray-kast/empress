use std::{
    collections::{BTreeSet, HashMap, HashSet},
    future::Future,
    time::Instant,
};

use futures_util::{stream::FuturesUnordered, StreamExt};
use log::{debug, trace, warn};
use tokio::sync::RwLock;
use zbus::names::OwnedBusName;

use super::{mpris::player::PlaybackStatus, Player};
use crate::Result;

#[derive(Debug)]
pub(super) struct PlayerMap(
    HashMap<OwnedBusName, (PlaybackStatus, Instant)>,
    BTreeSet<Player>,
);

impl PlayerMap {
    pub async fn inform<P: Fn(&OwnedBusName) -> PR + Copy, PR: Future<Output = Result<Player>>>(
        this: &RwLock<Self>,
        try_patch: bool,
        names: HashSet<OwnedBusName>,
        player: P,
    ) {
        use std::collections::hash_map::Entry;

        let key_set = this.read().await.0.keys().cloned().collect();

        if try_patch {
            let vec = names
                .intersection(&key_set)
                .map(|n| async move { (n, player(n).await) })
                .collect::<FuturesUnordered<_>>()
                .collect::<Vec<_>>()
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
                let (status, last_update) = this.0.get(name).unwrap();

                if player.status != *status || player.last_update < *last_update {
                    this.put(player);
                } else {
                    trace!("Skipping map update for player: {:?}", player);
                }
            }

            for name in key_set.difference(&names) {
                assert!(this.remove(name).is_some());
            }
        } else {
            let mut this = this.write().await;

            for name in names.symmetric_difference(&key_set) {
                match this.0.entry(name.clone()) {
                    Entry::Vacant(v) => {
                        let player = match player(v.key()).await {
                            Ok(p) => p,
                            Err(e) => {
                                warn!("Constructing player failed: {:?}", e);
                                continue;
                            },
                        };

                        trace!("Quick-adding new player to map: {:?}", player);

                        v.insert((player.status, player.last_update));
                        this.1.insert(player);
                    },
                    Entry::Occupied(o) => {
                        let (bus, (status, last_update)) = o.remove_entry();

                        trace!("Quick-removing player from map: {:?}", bus);

                        assert!(this.1.remove(&Player {
                            status,
                            last_update,
                            bus
                        }));
                    },
                }
            }
        }
    }

    pub fn new() -> Self { Self(HashMap::new(), BTreeSet::new()) }

    pub fn iter_all(&self) -> impl Iterator<Item = &Player> { self.1.iter() }

    /// Returns all players whose status matches that of the first (i.e.
    /// highest-priority) player in the list.  This should be used when
    /// selecting a player to run an action on, to avoid unexpected behavior.
    pub fn iter_active(&self) -> impl Iterator<Item = &Player> {
        let mut prev = None;

        self.1.iter().take_while(move |p| {
            let ret = prev.map_or(true, |s| s == p.status);
            prev = Some(p.status);

            if !ret {
                debug!("Stopping active-player search before {:?}", p);
            }

            ret
        })
    }

    /// Always updates the map, but only returns true if a new key was inserted
    pub fn put(&mut self, player: Player) -> bool {
        use std::collections::hash_map::Entry;

        match self.0.entry(player.bus.clone()) {
            Entry::Vacant(v) => {
                trace!("Inserting new player into map: {:?}", player);

                v.insert((player.status, player.last_update));
                self.1.insert(player);
                true
            },
            Entry::Occupied(o) => {
                trace!("Patching existing player in map: {:?}", player);

                let (status, last_update) = *o.get();
                assert!(self.1.remove(&Player {
                    status,
                    last_update,
                    bus: player.bus.clone()
                }));
                *o.into_mut() = (player.status, player.last_update);
                self.1.insert(player);
                false
            },
        }
    }

    pub fn remove(&mut self, bus: &OwnedBusName) -> Option<Player> {
        if let Some((status, last_update)) = self.0.remove(bus) {
            trace!("Removing player from map: {:?}", bus);

            let ply = Player {
                status,
                last_update,
                bus: bus.clone(),
            };

            assert!(self.1.remove(&ply));

            Some(ply)
        } else {
            trace!("Player to remove does not exist in map: {:?}", bus);

            None
        }
    }
}
