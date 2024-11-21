use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1},
    combinator::{eof, map},
    error::ErrorKind,
    multi::many0,
    sequence::tuple,
    Finish, IResult, InputTakeAtPosition,
};
use serde_json::Number;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Pos(usize);

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

pub type SpannedTok<'a> = (Pos, Token<'a>, Pos);

#[derive(Debug, Clone)]
pub enum Token<'a> {
    Fragment(&'a str),
    BlockEnd,

    Dot,
    Comma,
    PipeBang,
    Pipe,
    Coalesce,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Ident(&'a str),
    Number(Number),
    String(String),
}

impl<'a> Token<'a> {
    fn try_from_bit(bit: Bit<'a>) -> Option<Self> {
        Some(match bit {
            Bit::Fragment(s) => Token::Fragment(s),
            Bit::BlockEnd => Token::BlockEnd,

            Bit::Dot => Token::Dot,
            Bit::Comma => Token::Comma,
            Bit::PipeBang => Token::PipeBang,
            Bit::Pipe => Token::Pipe,
            Bit::Coalesce => Token::Coalesce,
            Bit::LParen => Token::LParen,
            Bit::RParen => Token::RParen,
            Bit::LBrack => Token::LBrack,
            Bit::RBrack => Token::RBrack,
            Bit::Ident(s) => Token::Ident(s),
            Bit::Number(n) => Token::Number(n),
            Bit::String(s) => Token::String(s),

            Bit::Whitespace | Bit::BlockStart => None?,
        })
    }
}

#[derive(Debug, Clone)]
enum Bit<'a> {
    Fragment(&'a str),
    BlockStart,
    BlockEnd,

    Whitespace,
    Dot,
    Comma,
    PipeBang,
    Pipe,
    Coalesce,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Ident(&'a str),
    Number(Number),
    String(String),
}

type SizedBit<'a> = (usize, Bit<'a>);
type BResult<'a, E = nom::error::Error<&'a str>> = IResult<&'a str, SizedBit<'a>, E>;

//////// Entry point

pub fn scan(s: &str) -> Result<Vec<SpannedTok>, nom::error::Error<&str>> {
    tokens(s).finish().map(|(_, v)| v)
}

//////// Helpers

#[inline]
fn bit<'a, 'b>(
    s: &'a str,
    t: &'b str,
    f: impl FnOnce(&'b str) -> Bit<'a>,
) -> (&'a str, SizedBit<'a>) {
    (s, (t.chars().count(), f(t)))
}

#[inline]
fn try_bit<'a, 'b, E: nom::error::FromExternalError<&'a str, F>, F>(
    input: &'a str,
    s: &'a str,
    t: &'b str,
    f: impl FnOnce(&'b str) -> Result<Bit<'a>, F>,
) -> BResult<'a, E> {
    match f(t) {
        Ok(bit) => Ok((s, (t.chars().count(), bit))),
        Err(err) => Err(nom::Err::Error(E::from_external_error(
            input,
            ErrorKind::MapRes,
            err,
        ))),
    }
}

fn btag(val: &'static str, b: Bit<'static>) -> impl for<'a> Fn(&'a str) -> BResult<'a> {
    move |s| {
        let (s, t) = tag(val)(s)?;

        Ok(bit(s, t, |_| b.clone()))
    }
}

fn any1(s: &str) -> IResult<&str, &str> { s.split_at_position1_complete(|_| true, ErrorKind::Char) }

// Adapted from split_at_position1_complete
fn split_before<'a, O, E>(
    allow_empty: bool,
    mut parser: impl nom::Parser<&'a str, O, E>,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    #[allow(clippy::unnecessary_wraps)]
    #[inline]
    unsafe fn ok(s: &str, i: usize) -> IResult<&str, &str> {
        Ok((s.get_unchecked(i..), s.get_unchecked(..i)))
    }

    #[inline]
    fn err(input: &str) -> IResult<&str, &str> {
        Err(nom::Err::Error(nom::error::Error {
            input,
            code: ErrorKind::Eof,
        }))
    }

    move |s| match s.char_indices().find_map(|(i, _)| {
        // char_indices() returns an index that is already
        // a char boundary
        let s = unsafe { s.get_unchecked(i..) };

        if parser.parse(s).is_ok() && (allow_empty || i != 0) {
            Some(i)
        } else {
            None
        }
    }) {
        Some(0) => err(s),
        // char_indices() returns a byte index that is already
        // in the slice at a char boundary
        Some(i) => unsafe { ok(s, i) },
        None => {
            if s.is_empty() {
                err(s)
            } else {
                // The end of slice is a char boundary
                unsafe { ok(s, s.len()) }
            }
        },
    }
}

//////// Lexer internals

fn tokens(s: &str) -> IResult<&str, Vec<SpannedTok>> {
    let (s, r) = many0(token())(s)?;
    let (s, _) = eof(s)?;

    Ok((
        s,
        r.into_iter()
            .filter_map({
                let mut pos = 0;

                move |(len, bit)| {
                    let ret = Token::try_from_bit(bit);

                    let start = Pos(pos);
                    pos += len;
                    let end = Pos(pos);

                    ret.map(|t| (start, t, end))
                }
            })
            .collect(),
    ))
}

fn token() -> impl FnMut(&str) -> BResult {
    let mut in_block = false;

    move |s| {
        if in_block {
            let (s, t) = alt((
                btag("}}", Bit::BlockEnd),
                space,
                btag(".", Bit::Dot),
                btag(",", Bit::Comma),
                btag("|!", Bit::PipeBang),
                btag("|", Bit::Pipe),
                btag("??", Bit::Coalesce),
                btag("(", Bit::LParen),
                btag(")", Bit::RParen),
                btag("[", Bit::LBrack),
                btag("]", Bit::RBrack),
                ident,
                float,
                int,
                string("'"),
                string("\""),
            ))(s)?;

            in_block = !matches!(t.1, Bit::BlockEnd);

            Ok((s, t))
        } else {
            let mut terminator = alt((escape, btag("{{", Bit::BlockStart)));

            match terminator(s) {
                Err(_) => {
                    // Performance note: this is possibly prone to quadratic
                    //                   backtracking, but the terminator isn't
                    //                   very long so hopefully it's not a huge
                    //                   deal.
                    let (s, t) = split_before(false, terminator)(s)?;

                    Ok(bit(s, t, Bit::Fragment))
                },
                Ok((s, t)) => {
                    in_block = matches!(t.1, Bit::BlockStart);
                    Ok((s, t))
                },
            }
        }
    }
}

fn escape(s: &str) -> BResult {
    let (s, _) = tag("\\")(s)?;
    let (s, c) = any1(s)?;

    Ok(bit(s, c, Bit::Fragment))
}

fn space(s: &str) -> BResult {
    let (s, t) = nom::character::complete::space1(s)?;

    Ok(bit(s, t, |_| Bit::Whitespace))
}

fn ident(s: &str) -> BResult {
    #[allow(clippy::unnecessary_wraps)]
    #[inline]
    unsafe fn ok(s: &str, i: usize) -> BResult {
        Ok(bit(s.get_unchecked(i..), s.get_unchecked(..i), Bit::Ident))
    }

    #[inline]
    fn err(input: &str) -> BResult {
        Err(nom::Err::Error(nom::error::Error {
            input,
            code: ErrorKind::Eof,
        }))
    }

    // Adapted from split_at_position1_complete
    match s.char_indices().find_map(|(i, c)| {
        let matches = matches!(c, '_' | '$')
            || if i == 0 {
                c.is_alphabetic()
            } else {
                c.is_alphanumeric()
            };

        if matches {
            None
        } else {
            Some(i)
        }
    }) {
        Some(0) => err(s),
        // char_indices() returns a byte index that is already
        // in the slice at a char boundary
        Some(i) => unsafe { ok(s, i) },
        None => {
            if s.is_empty() {
                err(s)
            } else {
                // The end of slice is a char boundary
                unsafe { ok(s, s.len()) }
            }
        },
    }
}

fn int(s: &str) -> BResult {
    let (s2, t) = digit1(s)?;

    try_bit(s, s2, t, |t| Number::from_str(t).map(Bit::Number))
}

fn float(s: &str) -> BResult {
    let (s2, (l, c, r)) = tuple((digit1, char('.'), digit1))(s)?;

    try_bit(s, s2, &format!("{l}{c}{r}"), |t| {
        Number::from_str(t).map(Bit::Number)
    })
}

fn string(delim: &'static str) -> impl for<'a> FnMut(&'a str) -> BResult<'a> {
    move |s| {
        let delim = tag(delim);
        let (mut s, t) = delim(s)?;

        // NOTE: this is the length of the input token, not out.len()!
        let mut len = t.len();
        let mut out = String::new();

        let mut terminator = alt((
            map(tuple((tag("\\"), any1)), |(t, c)| {
                (Some(c), t.len() + c.len())
            }),
            map(delim, |s| (None, s.len())),
        ));

        loop {
            match terminator(s) {
                Err(_) => {
                    let (s2, t) = split_before(false, &mut terminator)(s)?;
                    s = s2;
                    len += t.len();
                    out.push_str(t);
                },
                Ok((s2, (Some(c), l))) => {
                    s = s2;
                    len += l;
                    out.push_str(c);
                },
                Ok((s2, (None, l))) => {
                    s = s2;
                    len += l;
                    break;
                },
            }
        }

        Ok((s, (len, Bit::String(out))))
    }
}
