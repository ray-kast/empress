use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1},
    combinator::eof,
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
    Pipe,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Ident(&'a str),
    Number(Number),
}

impl<'a> Token<'a> {
    fn try_from_bit(bit: Bit<'a>) -> Option<Self> {
        Some(match bit {
            Bit::Fragment(s) => Token::Fragment(s),
            Bit::BlockEnd => Token::BlockEnd,

            Bit::Dot => Token::Dot,
            Bit::Comma => Token::Comma,
            Bit::Pipe => Token::Pipe,
            Bit::LParen => Token::LParen,
            Bit::RParen => Token::RParen,
            Bit::LBrack => Token::LBrack,
            Bit::RBrack => Token::RBrack,
            Bit::Ident(s) => Token::Ident(s),
            Bit::Number(n) => Token::Number(n),

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
    Pipe,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Ident(&'a str),
    Number(Number),
}

type SizedBit<'a> = (usize, Bit<'a>);
type BResult<'a, E = nom::error::Error<&'a str>> = IResult<&'a str, SizedBit<'a>, E>;

//// Entry point

pub fn scan(s: &str) -> Result<Vec<SpannedTok>, nom::error::Error<&str>> {
    tokens(s).finish().map(|(_, v)| v)
}

//// Helpers

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

//// Lexer internals

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
                btag("|", Bit::Pipe),
                btag("(", Bit::LParen),
                btag(")", Bit::RParen),
                btag("[", Bit::LBrack),
                btag("]", Bit::RBrack),
                ident,
                float,
                int,
            ))(s)?;

            in_block = !matches!(t.1, Bit::BlockEnd);

            Ok((s, t))
        } else {
            let mut terminator = alt((escape, btag("{{", Bit::BlockStart)));

            match terminator(s) {
                Err(_) => {
                    #[allow(clippy::unnecessary_wraps)]
                    unsafe fn ok(s: &str, i: usize) -> BResult {
                        Ok(bit(
                            s.get_unchecked(i..),
                            s.get_unchecked(..i),
                            Bit::Fragment,
                        ))
                    }

                    fn err(input: &str) -> BResult {
                        Err(nom::Err::Error(nom::error::Error {
                            input,
                            code: ErrorKind::Eof,
                        }))
                    }

                    // Adapted from split_at_position1_complete

                    // Performance note: this is possibly prone to quadratic
                    //                   backtracking, but the terminator isn't
                    //                   very long so hopefully it's not a huge
                    //                   deal.
                    match s.char_indices().find_map(|(i, _)| {
                        // char_indices() returns an index that is already
                        // a char boundary
                        let s = unsafe { s.get_unchecked(i..) };

                        if terminator(s).is_ok() { Some(i) } else { None }
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
    let (s, c) = s.split_at_position1_complete(|_| true, ErrorKind::Eof)?;

    Ok(bit(s, c, Bit::Fragment))
}

fn space(s: &str) -> BResult {
    let (s, t) = nom::character::complete::space1(s)?;

    Ok(bit(s, t, |_| Bit::Whitespace))
}

fn ident(s: &str) -> BResult {
    #[allow(clippy::unnecessary_wraps)]
    unsafe fn ok(s: &str, i: usize) -> BResult {
        Ok(bit(s.get_unchecked(i..), s.get_unchecked(..i), Bit::Ident))
    }

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

        if matches { None } else { Some(i) }
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

    try_bit(s, s2, &format!("{}{}{}", l, c, r), |t| {
        Number::from_str(t).map(Bit::Number)
    })
}
