use std::borrow::Cow::{Borrowed, Owned};

use super::interp::{Context, CowValue, Error, Eval, Result, Stream, Value};

#[derive(Debug)]
pub enum Segment<'a> {
    Fragment(&'a str),
    Block(Option<Expr<'a>>),
}

impl<'s> Stream for Segment<'s> {
    fn stream(self, ctx: &Context, mut out: impl std::fmt::Write) -> Result<()> {
        match self {
            Self::Fragment(s) => out.write_str(s).map_err(Into::into),
            Self::Block(e) => e.map_or(Ok(()), |e| e.eval(ctx, None)?.as_ref().stream(ctx, out)),
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct Expr<'a>(pub Box<Pipeline<'a>>);

impl<'a, 's> Eval<'a> for Expr<'s> {
    type Output = <Pipeline<'s> as Eval<'a>>::Output;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<Self::Output> {
        self.0.eval(ctx, topic)
    }
}

#[derive(Debug)]
pub enum Pipeline<'a> {
    Pipe(Box<Pipeline<'a>>, Prim<'a>),
    Prim(Prim<'a>),
}

impl<'a, 's> Eval<'a> for Pipeline<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Pipe(p, r) => r.eval(ctx, Some(p.eval(ctx, topic)?)),
            Self::Prim(r) => r.eval(ctx, topic),
        }
    }
}

#[derive(Debug)]
pub enum Prim<'a> {
    Path(Path<'a>),
    Call(&'a str, Args<'a>),
    Value(Value),
}

impl<'a, 's> Eval<'a> for Prim<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Path(p) => p.eval(ctx, topic),
            Self::Call(i, a) => ctx
                .functions
                .get(i)
                .ok_or_else(|| Error::NoFunction(i.into()))?(
                topic, a.eval(ctx, None)?
            ),
            Self::Value(v) => Ok(Owned(v)),
        }
    }
}

#[derive(Debug)]
pub enum Path<'a> {
    Dot(Box<Path<'a>>, &'a str),
    Ident(&'a str),
}

impl<'a, 's> Eval<'a> for Path<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Dot(l, r) => match l.eval(ctx, topic)? {
                Owned(Value::Object(mut m)) if m.contains_key(r) => {
                    Ok(Owned(m.remove(r).unwrap_or_else(|| unreachable!())))
                },
                Borrowed(Value::Object(m)) if m.contains_key(r) => {
                    Ok(Borrowed(m.get(r).unwrap_or_else(|| unreachable!())))
                },
                l => Err(Error::BadPath(l.into_owned(), r.into())),
            },
            Self::Ident(i) => match ctx.values.get(i) {
                Some(v) => Ok(Borrowed(v)),
                None => Err(Error::NoValue(i.into())),
            },
        }
    }
}

#[derive(Debug)]
pub enum Args<'a> {
    Comma(Box<Args<'a>>, Expr<'a>),
    Expr(Expr<'a>),
}

impl<'a, 's> Eval<'a> for Args<'s> {
    type Output = Vec<CowValue<'a>>;

    fn eval(self, ctx: &'a Context, _: Option<CowValue<'a>>) -> Result<Vec<CowValue<'a>>> {
        match self {
            Self::Comma(l, r) => {
                let mut vec = l.eval(ctx, None)?;
                vec.push(r.eval(ctx, None)?);
                Ok(vec)
            },
            Self::Expr(e) => e.eval(ctx, None).map(|v| vec![v]),
        }
    }
}
