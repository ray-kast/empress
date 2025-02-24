use std::{
    borrow::Cow::{Borrowed, Owned},
    collections::HashMap,
    fmt::Write,
    sync::LazyLock,
};

use anyhow::Context;
use regex::Regex;

use super::{
    ffi::{Any, Array, Error, Function, Input, NoTopic, Number, Output, Result, Topic},
    interp::{is_null_like, stringify, write_value, Value},
};
use crate::server::mpris::player::PlaybackStatus;

pub type Functions = HashMap<&'static str, Function>;

// TODO: add pad function
pub fn all() -> Functions {
    vec![
        ("blank", blank as Function),
        ("compact", compact),
        ("eta", eta),
        ("join", join),
        ("json", json),
        ("lower", lower),
        ("percent", percent),
        ("shorten", shorten),
        ("shortenMid", shorten_mid),
        ("symbol", symbol),
        ("time", time),
        ("trim", trim),
        ("upper", upper),
        ("xml", xml),
    ]
    .into_iter()
    .collect()
}

//////// Helper functions

fn is_blank(value: &Value) -> bool {
    match value {
        Value::Null => true,
        Value::Bool(b) => !b,
        Value::Number(n) => {
            n.as_i128().is_some_and(|i| i == 0) || n.as_f64().is_some_and(|f| f.abs() < 1e-7)
        },
        Value::String(s) => s.trim_start().is_empty(),
        Value::Array(a) => a.iter().all(is_blank),
        Value::Object(o) => o.is_empty(),
    }
}

fn dhmss_micros(mut micros: i64, neg_zero: bool) -> String {
    let mut s = String::new();

    if micros < 0 || micros == 0 && neg_zero {
        s.push('-');
        micros = -micros;
    }

    let mut sec = micros / 1_000_000;
    let mut min = sec / 60;
    sec %= 60;
    let mut hr = min / 60;
    min %= 60;
    let day = hr / 24;
    hr %= 24;

    if day > 365 {
        // Probably not worth displaying anything at this point, just put an
        // infinity symbol
        write!(s, "\u{221e}").unwrap();
    } else if day > 0 {
        write!(s, "{day:01}:{hr:02}:{min:02}:{sec:02}").unwrap();
    } else if hr > 0 {
        write!(s, "{hr:01}:{min:02}:{sec:02}").unwrap();
    } else {
        write!(s, "{min:01}:{sec:02}").unwrap();
    }

    s
}

#[inline]
fn stream_str(inp: Input, f: impl FnOnce(String) -> String) -> Output {
    let (_ctx, Topic(Any(v)), ()) = inp.try_into()?;

    Ok(Owned(Value::String(f(stringify(v)?))))
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

//////// Format function definitions

fn blank(inp: Input) -> Output {
    let (_ctx, NoTopic, (Any(val), ())) = inp.try_into()?;

    Ok(Owned(Value::Bool(is_blank(val.as_ref()))))
}

fn compact(inp: Input) -> Output {
    let (_ctx, Topic(Array(arr)), ()) = inp.try_into()?;

    Ok(Owned(match arr {
        Owned(a) => a.into_iter().filter(|e| !is_null_like(e)).collect(),
        Borrowed(a) => a.iter().filter(|e| !is_null_like(e)).cloned().collect(),
    }))
}

fn eta(inp: Input) -> Output {
    let (_ctx, Topic(Number::<i64>(len)), (Number::<i64>(duration), ())) = inp.try_into()?;

    Ok(Owned(Value::String(dhmss_micros(len - duration, true))))
}

fn join(inp: Input) -> Output {
    let (_ctx, Topic(Array(arr)), (Any(sep), ())) = inp.try_into()?;

    let mut s = String::new();

    for (i, el) in arr.iter().enumerate() {
        if i > 0 {
            write_value(sep.as_ref(), &mut s)?;
        }

        write_value(el, &mut s)?;
    }

    Ok(Owned(Value::String(s)))
}

fn json(inp: Input) -> Output {
    let (_ctx, Topic(Any(v)), ()) = inp.try_into()?;

    Ok(Owned(Value::String(
        serde_json::to_string(v.as_ref()).context("Error serializing JSON")?,
    )))
}

fn lower(inp: Input) -> Output { stream_str(inp, |s| s.to_lowercase()) }

fn percent(inp: Input) -> Output {
    let (_ctx, Topic(Number::<f64>(n)), ()) = inp.try_into()?;

    Ok(Owned(Value::String(if n.is_finite() {
        let pct = (n * 100.0).round();
        let pct = if n <= 1.0 {
            if n - 1.0 > -1e-7 {
                100.0
            } else {
                pct.min(99.0)
            }
        } else {
            pct
        };

        format!("{pct}%")
    } else {
        "\u{2014}%".into()
    })))
}

fn shorten(inp: Input) -> Output {
    let (_ctx, Topic(Any(val)), (Number::<usize>(len), (Any(ell), ()))) = inp.try_into()?;

    let val = stringify(val)?;
    let ell = stringify(ell)?;
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

    let val = stringify(val)?;
    let ell = stringify(ell)?;
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

fn symbol(inp: Input) -> Output {
    stream_str(inp, |s| match s.parse() {
        Ok(PlaybackStatus::Playing) => "\u{25b6}".into(),
        Ok(PlaybackStatus::Paused) => "\u{23f8}".into(),
        Ok(PlaybackStatus::Stopped) => "\u{23f9}".into(),
        Err(_) => s,
    })
}

fn time(inp: Input) -> Output {
    let (_ctx, Topic(Number::<i64>(len)), ()) = inp.try_into()?;

    Ok(Owned(Value::String(dhmss_micros(len, false))))
}

fn trim(inp: Input) -> Output { stream_str(inp, |s| s.trim().to_owned()) }

fn upper(inp: Input) -> Output { stream_str(inp, |s| s.to_uppercase()) }

fn xml(inp: Input) -> Output {
    static ENTITY_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"['"\&<>]"#).unwrap());

    static REPLACE_MAP: LazyLock<HashMap<&'static str, &'static str>> = LazyLock::new(|| {
        vec![
            ("'", "apos"),
            ("\"", "quot"),
            ("&", "amp"),
            ("<", "lt"),
            (">", "gt"),
        ]
        .into_iter()
        .collect()
    });

    stream_str(inp, |s| {
        match ENTITY_RE.replace_all(&s, |c: &regex::Captures| {
            format!("&{};", REPLACE_MAP[&c[0]])
        }) {
            Borrowed(_) => s,
            Owned(o) => o,
        }
    })
}
