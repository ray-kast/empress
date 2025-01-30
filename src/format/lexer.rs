use std::{borrow::Cow, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, multispace0, multispace1, newline, satisfy},
    combinator::{eof, map, recognize},
    error::{Error, ErrorKind, FromExternalError, ParseError},
    multi::{many0, many1},
    Err, Finish, IResult, Input, Mode, Parser,
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
    Fragment(Cow<'a, str>),
    BlockEnd,

    KwElif,
    KwElse,
    KwEnd,
    KwFalse,
    KwIf,
    KwLet,
    KwPut,
    KwTrue,

    Dot,
    Bang,
    Comma,
    PipeBang,
    Pipe,
    Coalesce,
    LParen,
    RParen,
    LBrack,
    RBrack,

    Or,
    And,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,

    Dollar,
    Declare,

    Ident(&'a str),
    Number(Number),
    String(String),
}

impl<'a> Token<'a> {
    fn try_from_bit(bit: Bit<'a>) -> Option<Self> {
        match bit {
            Bit::Token(t) => Some(t),
            Bit::Whitespace | Bit::BlockStart => None,
        }
    }
}

#[derive(Debug, Clone)]
enum Bit<'a> {
    BlockStart,
    Whitespace,
    Token(Token<'a>),
}

impl<'a> From<Token<'a>> for Bit<'a> {
    #[inline]
    fn from(value: Token<'a>) -> Self { Self::Token(value) }
}

type SizedBit<'a> = (usize, Bit<'a>);
type BResult<'a, E = Error<&'a str>> = IResult<&'a str, SizedBit<'a>, E>;

//////// Entry point

pub fn scan(s: &str) -> Result<Vec<SpannedTok>, Error<&str>> {
    tokens(s, token()).finish().map(|(_, v)| v)
}

pub fn scan_extended(s: &str) -> Result<Vec<SpannedTok>, Error<&str>> {
    tokens(s, ext_token()).finish().map(|(_, v)| v)
}

//////// Helpers

#[inline]
fn bit<'a, 'b, F: FnOnce(&'b str) -> B, B: Into<Bit<'a>>>(
    s: &'a str,
    t: &'b str,
    f: F,
) -> (&'a str, SizedBit<'a>) {
    (s, (t.chars().count(), f(t).into()))
}

#[inline]
fn try_bit<
    'a,
    'b,
    E: FromExternalError<&'a str, X>,
    F: FnOnce(&'b str) -> Result<B, X>,
    B: Into<Bit<'a>>,
    X,
>(
    input: &'a str,
    s: &'a str,
    t: &'b str,
    f: F,
) -> BResult<'a, E> {
    match f(t) {
        Ok(bit) => Ok((s, (t.chars().count(), bit.into()))),
        Err(err) => Err(Err::Error(E::from_external_error(
            input,
            ErrorKind::MapRes,
            err,
        ))),
    }
}

fn btag<B: Into<Bit<'static>>>(val: &'static str, b: B) -> impl for<'a> Fn(&'a str) -> BResult<'a> {
    let b = b.into();
    move |s| {
        let (s, t) = tag(val)(s)?;

        Ok(bit(s, t, |_| b.clone()))
    }
}

//////// Lexer internals

fn tokens<T: FnMut(&str) -> BResult>(s: &str, tok: T) -> IResult<&str, Vec<SpannedTok>> {
    let (s, r) = many0(tok).parse_complete(s)?;
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
            let (s, t) = alt((btag("}}", Token::BlockEnd), common_token)).parse_complete(s)?;

            in_block = !matches!(t.1, Bit::Token(Token::BlockEnd));

            Ok((s, t))
        } else {
            let mut terminator = alt((
                Escape(["{"]).map(|(l, s)| (l, Token::Fragment(s).into())),
                btag("{{", Bit::BlockStart),
            ));

            match terminator.parse_complete(s) {
                Err(_) => {
                    let (s, t) =
                        s.split_at_position1_complete(|c| c == '{' || c == '\\', ErrorKind::Alt)?;

                    Ok(bit(s, t, |s| Token::Fragment(s.into())))
                },
                Ok((s, t)) => {
                    in_block = matches!(t.1, Bit::BlockStart);
                    Ok((s, t))
                },
            }
        }
    }
}

fn ext_token() -> impl FnMut(&str) -> BResult { common_token }

fn common_token(s: &str) -> BResult {
    let tags = alt([
        btag(".", Token::Dot),
        btag("!", Token::Bang),
        btag(",", Token::Comma),
        btag("|!", Token::PipeBang),
        btag("|", Token::Pipe),
        btag("??", Token::Coalesce),
        btag("(", Token::LParen),
        btag(")", Token::RParen),
        btag("[", Token::LBrack),
        btag("]", Token::RBrack),
        //
        btag("||", Token::Or),
        btag("&&", Token::And),
        btag("==", Token::Eq),
        btag("!=", Token::Neq),
        btag("<", Token::Lt),
        btag(">", Token::Gt),
        btag("<=", Token::Le),
        btag(">=", Token::Ge),
        //
        btag("$", Token::Dollar),
        btag(":=", Token::Declare),
    ]);
    alt((space, tags, ident, float, int, string("'"), string("\""))).parse_complete(s)
}

struct Escape<const LEN: usize>([&'static str; LEN]);

impl<'a, const LEN: usize> Parser<&'a str> for Escape<LEN> {
    type Error = Error<&'a str>;
    type Output = (usize, Cow<'a, str>);

    fn process<OM: nom::OutputMode>(
        &mut self,
        input: &'a str,
    ) -> nom::PResult<OM, &'a str, Self::Output, Self::Error> {
        #[inline]
        fn lit(s: &str) -> (usize, Cow<'_, str>) { (s.chars().count(), s.into()) }

        #[inline]
        fn fixed(o: &'static str) -> impl Fn(&str) -> (usize, Cow<'_, str>) {
            |s| (s.chars().count(), o.into())
        }

        fn hex<I: Input<Item: nom::AsChar>, E: ParseError<I>>(
        ) -> impl FnMut(I) -> IResult<I, char, E> {
            satisfy(|c| c.is_ascii_hexdigit())
        }

        let ansi_esc = (tag("x"), recognize((hex(), hex()))).map(|(s, x): (&str, &str)| {
            (
                s.chars().count() + x.chars().count(),
                char::from(u8::from_str_radix(x, 16).unwrap())
                    .to_string()
                    .into(),
            )
        });

        let uni_esc =
            (tag("u{"), recognize(many1(hex())), tag("}")).map(|(l, x, r): (&str, &str, &str)| {
                (
                    l.chars().count() + x.chars().count() + r.chars().count(),
                    char::try_from(u32::from_str_radix(x, 16).unwrap())
                        .unwrap()
                        .to_string()
                        .into(),
                )
            });

        let (s, t) = tag("\\").process::<OM>(input)?;
        let (s, o) = alt((
            alt(self.0.map(tag)).map(lit),
            tag("\\").map(lit),
            tag("0").map(fixed("\0")),
            tag("n").map(fixed("\n")),
            tag("r").map(fixed("\r")),
            tag("t").map(fixed("\t")),
            tag("e").map(fixed("\x1b")),
            recognize((newline, multispace0)).map(fixed("")),
            ansi_esc,
            uni_esc,
        ))
        .process::<OM>(s)?;
        Ok((
            s,
            OM::Output::combine(t, o, |t, (l, o)| (l + t.chars().count(), o)),
        ))
    }
}

fn space(s: &str) -> BResult {
    let (s, t) = multispace1(s)?;

    Ok(bit(s, t, |_| Bit::Whitespace))
}

fn ident(s: &str) -> BResult {
    let ident = recognize((
        satisfy(unicode_ident::is_xid_start),
        many0(satisfy(unicode_ident::is_xid_continue)),
    ))
    .parse_complete(s)
    .map(|(s, t)| bit(s, t, Token::Ident));

    let kw = alt([
        btag("elif", Token::KwElif),
        btag("else", Token::KwElse),
        btag("end", Token::KwEnd),
        btag("false", Token::KwFalse),
        btag("if", Token::KwIf),
        btag("let", Token::KwLet),
        btag("put", Token::KwPut),
        btag("true", Token::KwTrue),
    ])
    .parse_complete(s);

    match (kw, ident) {
        (Ok(k @ (_, (lk, _))), Ok(i @ (_, (li, _)))) => Ok(if li > lk { i } else { k }),
        (k @ Ok(_), Err(_)) => k,
        (Err(_), i) => i,
    }
}

fn int(s: &str) -> BResult {
    let (s2, t) = digit1(s)?;

    try_bit(s, s2, t, |t| Number::from_str(t).map(Token::Number))
}

fn float(s: &str) -> BResult {
    let (s2, f) = recognize((digit1, char('.'), digit1)).parse_complete(s)?;

    try_bit(s, s2, f, |t| Number::from_str(t).map(Token::Number))
}

fn string(delim: &'static str) -> impl for<'a> FnMut(&'a str) -> BResult<'a> {
    move |s| {
        let delim_tag = tag(delim);
        let delim_start = delim.chars().next().unwrap_or_else(|| unreachable!());
        let (mut s, t) = delim_tag(s)?;

        let mut tok_chars = t.chars().count();
        let mut out = String::new();

        let mut terminator = alt((
            Escape([delim]).map(|(l, s)| (Some(s), l)),
            map(delim_tag, |s| (None, s.chars().count())),
        ));

        loop {
            match terminator.parse_complete(s) {
                Err(_) => {
                    let (s2, t) = s.split_at_position1_complete(
                        |c| c == delim_start || c == '\\',
                        ErrorKind::Alt,
                    )?;
                    s = s2;
                    tok_chars += t.chars().count();
                    out.push_str(t);
                },
                Ok((s2, (Some(c), l))) => {
                    s = s2;
                    tok_chars += l;
                    out.push_str(c.as_ref());
                },
                Ok((s2, (None, l))) => {
                    s = s2;
                    tok_chars += l;
                    break;
                },
            }
        }

        Ok((s, (tok_chars, Token::String(out).into())))
    }
}
