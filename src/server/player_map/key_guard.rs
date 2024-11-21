use std::ops;

use super::{update_key, GetKey, KeyMap, PlayerKey};
use crate::server::player::Player;

#[derive(Debug)]
pub(super) struct KeyGuard(bool, KeyMap);

impl KeyGuard {
    pub const fn new() -> Self { Self(false, KeyMap::new()) }

    #[inline]
    fn check(&self) {
        assert!(!self.0, "PlayerMap was poisoned!");
    }
}

impl ops::Deref for KeyGuard {
    type Target = KeyMap;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.check();
        &self.1
    }
}

impl ops::DerefMut for KeyGuard {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.check();
        &mut self.1
    }
}

#[derive(Debug)]
pub(in crate::server) struct PlayerMut<'a> {
    old_key: PlayerKey,
    player: &'a mut Player,
    poison: &'a mut bool,
    keys: &'a mut KeyMap,
}

impl<'a> Drop for PlayerMut<'a> {
    fn drop(&mut self) {
        update_key(self.keys, &self.old_key, self.player);
        *self.poison = false;
    }
}

impl<'a> ops::Deref for PlayerMut<'a> {
    type Target = Player;

    #[inline]
    fn deref(&self) -> &Self::Target { self.player }
}

impl<'a> ops::DerefMut for PlayerMut<'a> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target { self.player }
}

impl<'a> PlayerMut<'a> {
    pub(super) fn new(player: &'a mut Player, keys: &'a mut KeyGuard) -> Self {
        let KeyGuard(poison, keys) = keys;
        *poison = true;

        Self {
            old_key: player.get_key(),
            player,
            poison,
            keys,
        }
    }
}
