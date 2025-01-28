use anyhow::Context;
use log::{trace, warn};
use zbus::{fdo::DBusProxy, MatchRule};

use crate::Result;

pub struct SignalMatcher<'a, I: IntoIterator<Item = MatchRule<'static>>> {
    dbus: DBusProxy<'a>,
    match_rules: Option<I>,
}

impl<'a, I: IntoIterator<Item = MatchRule<'static>> + Clone> SignalMatcher<'a, I> {
    pub async fn new(dbus: DBusProxy<'a>, match_rules: I) -> Result<Self> {
        for mr in match_rules.clone() {
            dbus.add_match_rule(mr.clone())
                .await
                .with_context(|| format!("Error adding match rule {mr}"))?;

            trace!("Match rule registered: {mr}");
        }

        Ok(Self {
            dbus,
            match_rules: Some(match_rules),
        })
    }
}

impl<I: IntoIterator<Item = MatchRule<'static>>> SignalMatcher<'_, I> {
    pub async fn shutdown(&mut self) -> Result {
        for mr in self
            .match_rules
            .take()
            .context("shutdown() called on already-closed signal matcher!")?
        {
            self.dbus
                .remove_match_rule(mr)
                .await
                .context("Error removing match rule")?;
        }

        Ok(())
    }
}

impl<I: IntoIterator<Item = MatchRule<'static>>> Drop for SignalMatcher<'_, I> {
    fn drop(&mut self) {
        // TODO: async drop wen eta son
        if self.match_rules.is_some() {
            warn!("Signal matcher dropped without calling .shutdown()!");
        }
    }
}
