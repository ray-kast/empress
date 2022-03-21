use std::{
    borrow::Cow::{Borrowed, Owned},
    collections::HashMap,
    fmt::Write,
};

use anyhow::Context;
use regex::Regex;

use super::{
    ffi::{Any, Array, Error, Function, Input, Number, Output, Result, Topic},
    interp::{is_null_like, Stream, StreamString, Value},
};
use crate::server::mpris::player::PlaybackStatus;

pub type Functions = HashMap<&'static str, Function>;

pub fn all() -> Functions {
    vec![
        ("compact", compact as Function),
        ("eta", eta),
        ("join", join),
        ("json", json),
        ("lower", lower),
        ("shorten", shorten),
        ("shortenMid", shorten_mid),
        ("sym", sym),
        ("time", time),
        ("trim", trim),
        ("upper", upper),
        ("xml", xml),
    ]
    .into_iter()
    .collect()
}

//// Helper functions

fn hmss_usec(mut us: i64, neg_zero: bool) -> String {
    let mut s = String::new();

    if us < 0 || us == 0 && neg_zero {
        s.push('-');
        us = -us;
    }

    let mut sec = us / 1_000_000;
    let mut min = sec / 60;
    sec %= 60;
    let hr = min / 60;
    min %= 60;

    if hr > 0 {
        write!(s, "{:01}:{:02}:{:02}", hr, min, sec).unwrap();
    } else {
        write!(s, "{:01}:{:02}", min, sec).unwrap();
    }

    s
}

#[inline]
fn stream_str(inp: Input, f: impl FnOnce(String) -> String) -> Output {
    let (_ctx, Topic(Any(v)), ()) = inp.try_into()?;

    Ok(Owned(Value::String(f(v.stream_string(())?))))
}

struct ShortenLen {
    input: usize,
    // ellipsis: usize,
    shortened: usize,
}

#[inline]
fn shorten_len(s: &str, ellipsis: &str, max_len: usize) -> Result<Option<ShortenLen>> {
    let ell_len = ellipsis.chars().count();

    if max_len < ell_len {
        return Err(Error::Other(anyhow::anyhow!(
            "Ellipsis string is longer than the maximum length"
        )));
    }

    let in_len = s.chars().count();

    Ok(if in_len > max_len {
        Some(ShortenLen {
            input: in_len,
            // ellipsis: ell_len,
            shortened: max_len
                .checked_sub(ell_len)
                .unwrap_or_else(|| unreachable!()),
        })
    } else {
        None
    })
}

//// Format function definitions

fn compact(inp: Input) -> Output {
    let (_ctx, Topic(Array(arr)), ()) = inp.try_into()?;

    Ok(Owned(match arr {
        Owned(a) => a.into_iter().filter(|e| !is_null_like(e)).collect(),
        Borrowed(a) => a.iter().cloned().filter(|e| !is_null_like(e)).collect(),
    }))
}

fn eta(inp: Input) -> Output {
    let (_ctx, Topic(Number::<i64>(len)), (Number::<i64>(duration), ())) = inp.try_into()?;

    Ok(Owned(Value::String(hmss_usec(len - duration, true))))
}

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

fn shorten(inp: Input) -> Output {
    let (_ctx, Topic(Any(val)), (Number::<usize>(len), (Any(ell), ()))) = inp.try_into()?;

    let val = val.stream_string(())?;
    let ell = ell.stream_string(())?;
    let len = shorten_len(&val, &ell, len)?;

    Ok(Owned(Value::String(match len {
        Some(len) => format!(
            "{}{}",
            unsafe {
                val.get_unchecked(
                    ..val
                        .char_indices()
                        .nth(len.shortened)
                        .unwrap_or_else(|| unreachable!())
                        .0,
                )
            },
            ell
        ),
        None => val,
    })))
}

fn shorten_mid(inp: Input) -> Output {
    let (_ctx, Topic(Any(val)), (Number::<usize>(len), (Any(ell), ()))) = inp.try_into()?;

    let val = val.stream_string(())?;
    let ell = ell.stream_string(())?;
    let len = shorten_len(&val, &ell, len)?;

    Ok(Owned(Value::String(match len {
        Some(ShortenLen {
            input,
            // ellipsis,
            shortened,
        }) => {
            let lower = shortened / 2;
            let upper = shortened - lower;

            let mut idcs = val.char_indices();

            let i = idcs.nth(upper).unwrap_or_else(|| unreachable!()).0;
            let j = if lower == 0 {
                val.len()
            } else {
                // NOTE: the iterator is sitting at upper + 1, so an extra - 1
                //       is necessary
                idcs.nth(input - shortened - 1)
                    .unwrap_or_else(|| unreachable!())
                    .0
            };

            unsafe {
                format!(
                    "{}{}{}",
                    val.get_unchecked(..i),
                    ell,
                    val.get_unchecked(j..)
                )
            }
        },
        None => val,
    })))
}

fn sym(inp: Input) -> Output {
    stream_str(inp, |s| match s.parse() {
        Ok(PlaybackStatus::Playing) => "▶".into(),
        Ok(PlaybackStatus::Paused) => "⏸".into(),
        Ok(PlaybackStatus::Stopped) => "⏹".into(),
        Err(_) => s,
    })
}

fn time(inp: Input) -> Output {
    let (_ctx, Topic(Number::<i64>(len)), ()) = inp.try_into()?;

    Ok(Owned(Value::String(hmss_usec(len, false))))
}

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
