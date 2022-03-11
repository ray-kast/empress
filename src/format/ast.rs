use std::borrow::Cow::{Borrowed, Owned};

use super::{
    ffi,
    interp::{assert_no_topic, Context, CowValue, Error, Eval, Result, Stream, Value},
};

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
    Pipe(Box<Pipeline<'a>>, Lens<'a>),
    Lens(Lens<'a>),
}

impl<'a, 's> Eval<'a> for Pipeline<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Pipe(p, l) => l.eval(ctx, Some(p.eval(ctx, topic)?)),
            Self::Lens(l) => l.eval(ctx, topic),
        }
    }
}

#[derive(Debug)]
pub enum Lens<'a> {
    Dot(Box<Lens<'a>>, &'a str),
    Index(Box<Lens<'a>>, Expr<'a>),
    Prim(Prim<'a>),
}

impl<'a, 's> Eval<'a> for Lens<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        fn as_usize(i: &Value) -> Option<usize> { i.as_u64().and_then(|i| i.try_into().ok()) }

        fn array_has_idx(a: &[Value], i: &Value) -> bool {
            as_usize(i).map_or(false, |i| a.len() > i)
        }

        fn object_has_idx(m: &serde_json::Map<String, Value>, i: &Value) -> bool {
            i.as_str().map_or(false, |i| m.contains_key(i))
        }

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
            Self::Index(l, r) => match (l.eval(ctx, topic)?, r.eval(ctx, None)?) {
                (Owned(Value::Array(mut a)), r) if array_has_idx(&a, &r) => Ok(Owned(
                    a.remove(as_usize(&r).unwrap_or_else(|| unreachable!())),
                )),
                (Borrowed(Value::Array(a)), r) if array_has_idx(a, &r) => {
                    Ok(Borrowed(&a[as_usize(&r).unwrap_or_else(|| unreachable!())]))
                },
                (Owned(Value::Object(mut m)), r) if object_has_idx(&m, &r) => Ok(Owned(
                    r.as_str()
                        .and_then(|r| m.remove(r))
                        .unwrap_or_else(|| unreachable!()),
                )),
                (Borrowed(Value::Object(m)), r) if object_has_idx(m, &r) => Ok(Borrowed(
                    r.as_str()
                        .and_then(|r| m.get(r))
                        .unwrap_or_else(|| unreachable!()),
                )),
                (l, r) => Err(Error::BadIndex(l.into_owned(), r.into_owned())),
            },
            Self::Prim(p) => p.eval(ctx, topic),
        }
    }
}

#[derive(Debug)]
pub enum Prim<'a> {
    Paren(Expr<'a>),
    Call(&'a str, Option<Args<'a>>),
    Ident(&'a str),
    Value(Value),
}

impl<'a, 's> Eval<'a> for Prim<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        if matches!(self, Self::Value(_)) {
            assert_no_topic(&topic, &self)?;
        }

        match self {
            Self::Paren(e) => e.eval(ctx, topic),
            Self::Call(i, a) => {
                ctx.functions
                    .get(i)
                    .ok_or_else(|| Error::NoFunction(i.into()))?(ffi::Input::new(
                    ctx,
                    topic,
                    a.map_or_else(|| Ok(vec![]), |a| a.eval(ctx, None))?,
                ))
                .map_err(|e| Error::Ffi(i.into(), e))
            },
            Self::Ident(i) => match topic {
                // A little hacky, but if a topic is present treat idents as a
                // call rather than a value
                topic @ Some(_) => Prim::Call(i, None).eval(ctx, topic),
                None => match ctx.values.get(i) {
                    Some(v) => Ok(Borrowed(v)),
                    None => Err(Error::NoValue(i.into())),
                },
            },
            Self::Value(v) => Ok(Owned(v)),
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

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<Vec<CowValue<'a>>> {
        assert_no_topic(&topic, &self)?;

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
