use std::{
    borrow::Borrow,
    cmp,
    collections::{BTreeSet, HashMap, HashSet},
    hash::Hash,
    time::Instant,
};

use anyhow::Context;
use dead_names::DeadNameMap;
use key_guard::KeyGuard;
pub(super) use key_guard::PlayerMut;
use log::{debug, trace, warn};
use zbus::{
    names::{BusName, UniqueName, WellKnownName},
    Connection,
};

use super::{mpris::player::PlaybackStatus, player::Action, Player};
use crate::Result;

mod dead_names;
mod key_guard;

// TODO: smoke-test map update logic

#[derive(Debug, PartialEq, Eq)]
struct PlayerKey {
    status: PlaybackStatus,
    last_update: Instant,
    bus: WellKnownName<'static>,
}

impl cmp::PartialOrd for PlayerKey {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> { Some(self.cmp(other)) }
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

pub enum PlayerChange {
    Add(WellKnownName<'static>),
    Refresh(WellKnownName<'static>),
    Remove(WellKnownName<'static>),
}

impl std::fmt::Display for PlayerChange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(n) => write!(f, "{n:?} added"),
            Self::Refresh(n) => write!(f, "{n:?} refreshed"),
            Self::Remove(n) => write!(f, "{n:?} removed"),
        }
    }
}

type KeyMap = BTreeSet<PlayerKey>;

fn active_keys(keys: &KeyMap) -> impl Iterator<Item = &PlayerKey> {
    let mut prev = None;

    keys.iter().take_while(move |p| {
        let ret = prev.map_or(true, |s| s == p.status);
        prev = Some(p.status);

        if !ret {
            debug!("Stopping active-player search before {p:?}");
        }

        ret
    })
}

fn update_key<Q: ?Sized + Ord, P: GetKey>(keys: &mut KeyMap, old: &Q, player: &P)
where PlayerKey: Borrow<Q> {
    let key = player.get_key();
    if key.borrow() != old {
        assert!(keys.remove(old));
        assert!(keys.insert(key));
    }
}

#[derive(Debug)]
pub(super) struct PlayerMap {
    players: HashMap<WellKnownName<'static>, Player>,
    names: HashMap<UniqueName<'static>, WellKnownName<'static>>,
    dead_names: DeadNameMap,
    keys: KeyGuard,
}

impl PlayerMap {
    pub async fn inform<'a>(
        &'a mut self,
        conn: &'a Connection,
        now: Instant,
        try_patch: bool,
        names: HashMap<UniqueName<'static>, WellKnownName<'static>>,
        mut changes: Option<&mut Vec<PlayerChange>>,
    ) {
        let curr_buses: HashSet<_> = self.players.keys().cloned().collect();
        let new_buses: HashSet<_> = names.values().cloned().collect();

        assert!(new_buses.len() == names.len());
        self.names = names;
        self.dead_names = DeadNameMap::new();

        for name in curr_buses.difference(&new_buses) {
            trace!("Removing player from map: {name:?}");

            if let Some(ref mut c) = changes {
                c.push(PlayerChange::Remove(name.clone()));
            }
            assert!(self.remove(name));
        }

        for name in new_buses.difference(&curr_buses) {
            let player = match Player::new(now, BusName::WellKnown(name.into()), conn).await {
                Ok(p) => p,
                Err(e) => {
                    warn!("Error constructing player: {e:?}");
                    continue;
                },
            };

            if let Some(ref mut c) = changes {
                c.push(PlayerChange::Add(name.clone()));
            }
            assert!(self.put(player).is_none());
        }

        if try_patch {
            for name in new_buses.intersection(&curr_buses) {
                let mut player = self.get_mut(name).unwrap();

                match player.refresh(now).await {
                    Ok(Some(_)) => {
                        if let Some(ref mut c) = changes {
                            c.push(PlayerChange::Refresh(name.clone()));
                        }
                    },
                    Ok(None) => (),
                    Err(e) => {
                        warn!("Error refreshing player: {e:?}");
                        continue;
                    },
                }
            }
        }
    }

    pub fn new() -> Self {
        Self {
            players: HashMap::new(),
            names: HashMap::new(),
            dead_names: DeadNameMap::new(),
            keys: KeyGuard::new(),
        }
    }

    pub fn resolve<Q: ?Sized + Hash + Eq>(&self, uniq: &Q) -> Option<&WellKnownName<'static>>
    where UniqueName<'static>: Borrow<Q> {
        self.names.get(uniq).or_else(|| self.dead_names.get(uniq))
    }

    pub fn iter_all(&self) -> impl Iterator<Item = &Player> {
        self.keys.iter().map(|k| &self.players[&k.bus])
    }

    /// Returns all players whose status matches that of the first (i.e.
    /// highest-priority) player in the list.  This should be used when
    /// selecting a player to run an action on, to avoid unexpected behavior.
    pub fn iter_active(&self) -> impl Iterator<Item = &Player> {
        active_keys(&self.keys).map(|k| &self.players[&k.bus])
    }

    pub fn contains_key<Q: ?Sized + Hash + Eq>(&mut self, bus: &Q) -> bool
    where WellKnownName<'static>: Borrow<Q> {
        self.players.contains_key(bus)
    }

    pub fn get_mut<Q: ?Sized + Hash + Eq>(&mut self, bus: &Q) -> Option<PlayerMut>
    where WellKnownName<'static>: Borrow<Q> {
        self.players
            .get_mut(bus)
            .map(|player| PlayerMut::new(player, &mut self.keys))
    }

    pub async fn apply_action<A: Action>(&mut self, action: A) -> Result<Option<A::Output>> {
        let mut keys = active_keys(&self.keys);
        let found = loop {
            let Some(key) = keys.next() else { break None };

            let player = self.players.get_mut(&key.bus).unwrap();
            if let Some(arg) = action.can_run(player).await.unwrap_or_else(|e| {
                warn!("Error querying player {:?} for action: {e:?}", key.bus);
                None
            }) {
                break Some((player, arg));
            }
        };
        let Some((player, arg)) = found else {
            return Ok(None);
        };
        drop(keys);
        let mut player = PlayerMut::new(player, &mut self.keys);

        action.run(&mut player, arg).await.map(Some)
    }

    /// Always updates the map, but only returns `Some` if an existing key was
    /// found and replaced
    pub fn put(&mut self, player: Player) -> Option<Player> {
        use std::collections::hash_map::Entry;

        match self.players.entry(player.bus().to_owned()) {
            Entry::Vacant(v) => {
                trace!("Inserting new player into map: {:?}", player.bus());

                assert!(self.keys.insert(player.get_key()));
                v.insert(player);
                None
            },
            Entry::Occupied(o) => {
                trace!("Patching existing player in map: {:?}", player.bus());

                let value = o.into_mut();
                update_key(&mut self.keys, &value.get_key(), &player);
                Some(std::mem::replace(value, player))
            },
        }
    }

    pub fn remove<Q: ?Sized + Hash + Eq + std::fmt::Debug>(&mut self, bus: &Q) -> bool
    where WellKnownName<'static>: Borrow<Q> {
        if let Some(old_player) = self.players.remove(bus) {
            trace!("Removing player from map: {bus:?}");

            assert!(self.keys.remove(&old_player.get_key()));

            true
        } else {
            warn!("Player to remove does not exist in map: {bus:?}");

            false
        }
    }

    pub async fn put_owner(
        &mut self,
        conn: &Connection,
        now: Instant,
        uniq: UniqueName<'static>,
        bus: WellKnownName<'static>,
    ) -> Result<(Option<WellKnownName<'static>>, bool)> {
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

                Ok((Some(old_bus), false))
            } else {
                warn!("Replacing bus name {old_bus:?} with {bus:?}");

                let ret = self.remove(&old_bus);

                insert_new(self, conn, now, bus).await?;

                Ok((Some(old_bus), ret))
            }
        } else {
            insert_new(self, conn, now, bus).await?;

            Ok((None, false))
        }
    }

    pub fn remove_owner<Q: ?Sized + Hash + Eq + ToOwned<Owned = UniqueName<'static>>>(
        &mut self,
        uniq: &Q,
    ) -> Option<(WellKnownName<'static>, bool)>
    where
        UniqueName<'static>: Borrow<Q>,
    {
        let bus = self.names.remove(uniq)?;
        let ret = self.remove(&bus);

        self.dead_names.insert(uniq.to_owned(), bus.clone());

        Some((bus, ret))
    }
}
