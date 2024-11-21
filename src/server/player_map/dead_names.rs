use std::borrow::Borrow;

use log::warn;
use zbus::names::{UniqueName, WellKnownName};

const LEN: usize = 8;

/// A quick and dirty log of released names to aid with race conditions.
// TODO(hack): this is.  bad.
#[derive(Debug)]
pub struct DeadNameMap {
    lru: [Option<(UniqueName<'static>, WellKnownName<'static>)>; LEN],
    free: usize,
}

impl DeadNameMap {
    pub fn new() -> Self {
        Self {
            lru: Default::default(),
            free: LEN.checked_sub(1).unwrap(),
        }
    }

    pub fn get<Q: ?Sized + Eq>(&self, uniq: &Q) -> Option<&WellKnownName<'static>>
    where UniqueName<'static>: Borrow<Q> {
        for i in self.free..(self.free + LEN) {
            let i = i % LEN;
            let Some((key, val)) = &self.lru[i] else {
                continue;
            };

            if key.borrow() == uniq {
                warn!("Resolved dead name {val:?}");
                return Some(val);
            }
        }

        None
    }

    pub fn insert(&mut self, uniq: UniqueName<'static>, bus: WellKnownName<'static>) {
        self.lru[self.free] = Some((uniq, bus));
        self.free = (self.free + LEN - 1) % LEN;
    }
}
