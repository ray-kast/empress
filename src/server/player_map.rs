use std::{
    borrow::Borrow,
    cmp,
    collections::{BTreeSet, HashMap, HashSet},
    hash::Hash,
    time::Instant,
};

use anyhow::Context;
use log::{debug, trace, warn};
use zbus::{
    names::{BusName, UniqueName, WellKnownName},
    Connection,
};

use super::{mpris::player::PlaybackStatus, Player};
use crate::Result;

// TODO: track changes to the current player (and its status) and ensure the map
//       fields remain in lock step after every mutation

#[derive(Debug, PartialEq, Eq)]
struct PlayerKey {
    status: PlaybackStatus,
    last_update: Instant,
    bus: WellKnownName<'static>,
}

impl cmp::PartialOrd for PlayerKey {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl cmp::Ord for PlayerKey {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.status
            .cmp(&other.status)
            .then_with(|| other.last_update.cmp(&self.last_update))
            .then_with(|| self.bus.cmp(&other.bus))
    }
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
    players: HashMap<WellKnownName<'static>, Player>,
    names: HashMap<UniqueName<'static>, WellKnownName<'static>>,
    keys: BTreeSet<PlayerKey>,
}

impl PlayerMap {
    pub async fn inform<'a>(
        &'a mut self,
        conn: &'a Connection,
        now: Instant,
        try_patch: bool,
        names: HashMap<UniqueName<'static>, WellKnownName<'static>>,
    ) {
        let key_set: HashSet<_> = self.players.keys().cloned().collect();
        let name_set: HashSet<_> = names.values().cloned().collect();

        assert!(name_set.len() == names.len());
        self.names = names;

        for name in key_set.difference(&name_set) {
            trace!("Removing player from map: {name:?}");

            assert!(self.remove(name).is_some());
        }

        for name in name_set.difference(&key_set) {
            let player = match Player::new(now, BusName::WellKnown(name.into()), conn).await {
                Ok(p) => p,
                Err(e) => {
                    warn!("Error constructing player: {e:?}");
                    continue;
                },
            };

            trace!("Adding new player to map: {player:?}");

            assert!(self.put(player).is_none());
        }

        if try_patch {
            for name in name_set.intersection(&key_set) {
                let mut player = self.players.remove(name).unwrap();

                match player.refresh().await {
                    Ok(()) => (),
                    Err(e) => {
                        warn!("Error refreshing player: {e:?}");
                        continue;
                    },
                }

                trace!("Updating player in map: {player:?}");
                assert!(self.put(player).is_none());
            }
        }
    }

    pub fn new() -> Self {
        Self {
            players: HashMap::new(),
            names: HashMap::new(),
            keys: BTreeSet::new(),
        }
    }

    pub fn resolve<Q: Hash + Eq>(&self, uniq: &Q) -> Option<&WellKnownName<'static>>
    where
        UniqueName<'static>: Borrow<Q>,
    {
        self.names.get(uniq)
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
                    debug!("Stopping active-player search before {p:?}");
                }

                ret
            })
            .map(|k| &self.players[&k.bus])
    }

    /// Always updates the map, but only returns `Some` if an existing key was
    /// found and replaced
    pub fn put(&mut self, player: Player) -> Option<Player> {
        use std::collections::hash_map::Entry;

        match self.players.entry(player.bus().to_owned()) {
            Entry::Vacant(v) => {
                trace!("Inserting new player into map: {player:?}");

                self.keys.insert(player.get_key());
                v.insert(player);
                None
            },
            Entry::Occupied(o) => {
                trace!("Patching existing player in map: {player:?}");

                let value = o.into_mut();
                assert!(self.keys.remove(&value.get_key()));
                self.keys.insert(player.get_key());
                Some(std::mem::replace(value, player))
            },
        }
    }

    pub fn remove<Q: Hash + Eq + std::fmt::Debug>(&mut self, bus: &Q) -> Option<Player>
    where
        WellKnownName<'static>: Borrow<Q>,
    {
        if let Some(old_player) = self.players.remove(bus) {
            trace!("Removing player from map: {bus:?}");

            assert!(self.keys.remove(&old_player.get_key()));

            Some(old_player)
        } else {
            trace!("Player to remove does not exist in map: {bus:?}");

            None
        }
    }

    pub async fn put_owner(
        &mut self,
        conn: &Connection,
        now: Instant,
        uniq: UniqueName<'static>,
        bus: WellKnownName<'static>,
    ) -> Result<(Option<WellKnownName<'static>>, Option<Player>)> {
        #[inline]
        async fn insert_new(
            this: &mut PlayerMap,
            conn: &Connection,
            now: Instant,
            bus: WellKnownName<'static>,
        ) -> Result<()> {
            if !this.players.contains_key(&bus) {
                assert!(this
                    .put(
                        Player::new(now, bus, conn)
                            .await
                            .context("Error constructing a new player")?,
                    )
                    .is_none());
            }

            Ok(())
        }

        if let Some(old_bus) = self.names.insert(uniq, bus.clone()) {
            if old_bus == bus {
                debug_assert!(self.players.contains_key(&bus));

                Ok((Some(old_bus), None))
            } else {
                let player = self.remove(&old_bus);

                insert_new(self, conn, now, bus).await?;

                Ok((Some(old_bus), player))
            }
        } else {
            insert_new(self, conn, now, bus).await?;

            Ok((None, None))
        }
    }

    pub fn remove_owner<Q: Hash + Eq>(
        &mut self,
        uniq: &Q,
    ) -> Option<(WellKnownName<'static>, Option<Player>)>
    where
        UniqueName<'static>: Borrow<Q>,
    {
        let bus = self.names.remove(uniq)?;
        let player = self.remove(&bus);

        Some((bus, player))
    }
}
