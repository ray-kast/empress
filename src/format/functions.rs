use std::{
    borrow::Cow::{Borrowed, Owned},
    collections::HashMap,
};

use anyhow::Context;
use regex::Regex;

use super::{
    ffi::{Any, Array, Function, Input, Output, Topic},
    interp::{Stream, Value},
};

pub type Functions = HashMap<&'static str, Function>;

pub fn all() -> Functions {
    vec![
        ("join", join as Function),
        ("json", json),
        ("lower", lower),
        ("trim", trim),
        ("upper", upper),
        ("xml", xml),
    ]
    .into_iter()
    .collect()
}

//// Helper functions

#[inline]
fn stream_str(inp: Input, f: impl FnOnce(String) -> String) -> Output {
    let (_ctx, Topic(Any(v)), ()) = inp.try_into()?;

    let mut s = String::new();
    v.stream((), &mut s)?;

    Ok(Owned(Value::String(f(s))))
}

//// Format function definitions

fn join(inp: Input) -> Output {
    let (_ctx, Topic(Array(arr)), (Any(sep), ())) = inp.try_into()?;

    let mut s = String::new();

    for (i, el) in arr.iter().enumerate() {
        if i > 0 {
            sep.stream((), &mut s)?;
        }

        el.stream((), &mut s)?;
    }

    Ok(Owned(Value::String(s)))
}

fn json(inp: Input) -> Output {
    let (_ctx, Topic(Any(v)), ()) = inp.try_into()?;

    Ok(Owned(Value::String(
        serde_json::to_string(v.as_ref()).context("Failed to serialize JSON")?,
    )))
}

fn lower(inp: Input) -> Output { stream_str(inp, |s| s.to_lowercase()) }

fn trim(inp: Input) -> Output { stream_str(inp, |s| s.trim().to_owned()) }

fn upper(inp: Input) -> Output { stream_str(inp, |s| s.to_uppercase()) }

fn xml(inp: Input) -> Output {
    lazy_static::lazy_static! {
        static ref ENTITY_RE: Regex = Regex::new(r#"['"\&<>]"#).unwrap();

        static ref REPLACE_MAP: HashMap<&'static str, &'static str> = vec![
                ("'", "apos"),
                ("\"", "quot"),
                ("&", "amp"),
                ("<", "lt"),
                (">", "gt"),
            ]
            .into_iter()
            .collect();
    }

    stream_str(inp, |s| {
        match ENTITY_RE.replace_all(&s, |c: &regex::Captures| {
            format!("&{};", REPLACE_MAP[&c[0]])
        }) {
            Borrowed(_) => s,
            Owned(o) => o,
        }
    })
}
