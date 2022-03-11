use std::{borrow::Cow::Owned, collections::HashMap};

use anyhow::Context;

use super::{
    ffi::{Any, Function, Input, Output, Topic},
    interp::Value,
};

pub type Functions = HashMap<&'static str, Function>;

pub fn all() -> Functions { vec![("json", json as Function)].into_iter().collect() }

fn json(inp: Input) -> Output {
    let (_ctx, Topic(Any(v)), ()) = inp.try_into()?;

    Ok(Owned(Value::String(
        serde_json::to_string(v.as_ref()).context("Failed to serialize JSON")?,
    )))
}
