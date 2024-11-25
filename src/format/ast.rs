use std::borrow::Cow::{Borrowed, Owned};

use super::{
    ffi,
    interp::{
        assert_no_topic, assert_topic, is_null_like, Context, CowValue, Error, Eval, Result,
        Stream, Value,
    },
};

#[derive(Debug)]
pub enum Segment<'a> {
    Fragment(&'a str),
    Block(Option<Expr<'a>>),
}

impl<'a, 's> Stream<'a> for Segment<'s> {
    type Context = &'a Context;
    type Error = Error;

    fn stream(self, ctx: &Context, mut out: impl std::fmt::Write) -> Result<()> {
        match self {
            Self::Fragment(s) => out.write_str(s).map_err(Into::into),
            Self::Block(e) => e.map_or(Ok(()), |e| {
                e.eval(ctx, None)?
                    .as_ref()
                    .stream((), out)
                    .map_err(Into::into)
            }),
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct Expr<'a>(pub Box<NullChain<'a>>);

impl<'a, 's> Eval<'a> for Expr<'s> {
    type Output = <NullChain<'s> as Eval<'a>>::Output;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<Self::Output> {
        self.0.eval(ctx, topic)
    }
}

#[derive(Debug)]
pub enum NullChain<'a> {
    Chain(Box<NullChain<'a>>, NullPipeline<'a>),
    Bang(NullPipeline<'a>),
}

impl<'a, 's> Eval<'a> for NullChain<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Chain(c, n) => match c.eval(ctx, topic.clone())? {
                Borrowed(v) if is_null_like(v) => n.eval(ctx, topic),
                Owned(v) if is_null_like(&v) => n.eval(ctx, topic),
                v => Ok(v),
            },
            Self::Bang(n) => n.eval(ctx, topic),
        }
    }
}

#[derive(Debug)]
pub enum NullPipeline<'a> {
    Bang(Box<NullPipeline<'a>>, Pipeline<'a>),
    Pipe(Pipeline<'a>),
}

impl<'a, 's> Eval<'a> for NullPipeline<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Bang(n, p) => match n.eval(ctx, topic)? {
                v @ (Owned(Value::Null) | Borrowed(Value::Null)) => Ok(v),
                v => p.eval(ctx, Some(v)),
            },
            Self::Pipe(p) => p.eval(ctx, topic),
        }
    }
}

#[derive(Debug)]
pub enum Pipeline<'a> {
    Pipe(Box<Pipeline<'a>>, Member<'a>),
    Member(Member<'a>),
}

impl<'a, 's> Eval<'a> for Pipeline<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Pipe(p, l) => l.eval(ctx, Some(p.eval(ctx, topic)?)),
            Self::Member(l) => l.eval(ctx, topic),
        }
    }
}

fn lens_ident<'a>(lhs: CowValue<'a>, rhs: &str) -> Result<CowValue<'a>> {
    match lhs {
        Owned(Value::Object(mut m)) if m.contains_key(rhs) => {
            Ok(Owned(m.remove(rhs).unwrap_or_else(|| unreachable!())))
        },
        Borrowed(Value::Object(m)) if m.contains_key(rhs) => {
            Ok(Borrowed(m.get(rhs).unwrap_or_else(|| unreachable!())))
        },
        l => Err(Error::BadPath(l.into_owned(), rhs.into())),
    }
}

fn lens_index<'a, R: Eval<'a, Output = CowValue<'a>>>(
    ctx: &'a Context,
    lhs: CowValue<'a>,
    rhs: R,
) -> Result<CowValue<'a>> {
    fn as_usize(i: &Value) -> Option<usize> {
        i.as_u64().and_then(|i| i.try_into().ok())
    }

    fn array_has_idx(a: &[Value], i: &Value) -> bool {
        as_usize(i).map_or(false, |i| a.len() > i)
    }

    fn object_has_idx(m: &serde_json::Map<String, Value>, i: &Value) -> bool {
        i.as_str().map_or(false, |i| m.contains_key(i))
    }

    match (lhs, rhs.eval(ctx, None)?) {
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
    }
}

#[derive(Debug)]
pub enum Member<'a> {
    Dot(Box<Member<'a>>, &'a str),
    Index(Box<Member<'a>>, Expr<'a>),
    Prim(Prim<'a>),
}

impl<'a, 's> Eval<'a> for Member<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Dot(l, r) => lens_ident(l.eval(ctx, topic)?, r),
            Self::Index(l, r) => lens_index(ctx, l.eval(ctx, topic)?, r),
            Self::Prim(p) => p.eval(ctx, topic),
        }
    }
}

#[derive(Debug)]
pub enum Prim<'a> {
    Paren(Expr<'a>),
    LensIdent(&'a str),
    LensIndex(Expr<'a>),
    Call(&'a str, Option<Args<'a>>),
    Array(Option<Args<'a>>),
    Ident(&'a str),
    Value(Value),
}

impl<'a, 's> Eval<'a> for Prim<'s> {
    type Output = CowValue<'a>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        if matches!(self, Self::Value(_)) {
            assert_no_topic(&topic, &self)?;
        }

        if matches!(self, Self::LensIdent(_) | Self::LensIndex(_)) {
            assert_topic(&topic, &self)?;
        }

        match self {
            Self::Paren(e) => e.eval(ctx, topic),
            Self::LensIdent(i) => lens_ident(topic.unwrap_or_else(|| unreachable!()), i),
            Self::LensIndex(e) => lens_index(ctx, topic.unwrap_or_else(|| unreachable!()), e),
            Self::Call(i, a) => {
                let Some(args) = a.map_or_else(|| Ok(Some(vec![])), |a| a.eval(ctx, None))? else {
                    return Ok(Owned(Value::Null));
                };

                ctx.functions
                    .get(i)
                    .ok_or_else(|| Error::NoFunction(i.into()))?(ffi::Input::new(
                    ctx, topic, args,
                ))
                .map_err(|e| Error::Ffi(i.into(), e))
            },
            Self::Array(a) => a
                .map_or_else(|| Ok(Some(vec![])), |a| a.eval(ctx, None))
                .map(|v| {
                    v.map_or(Owned(Value::Null), |v| {
                        Owned(Value::Array(
                            v.into_iter().map(CowValue::into_owned).collect(),
                        ))
                    })
                }),
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
    Comma(Box<Args<'a>>, Arg<'a>),
    Arg(Arg<'a>),
}

impl<'a, 's> Eval<'a> for Args<'s> {
    type Output = Option<Vec<CowValue<'a>>>;

    fn eval(
        self,
        ctx: &'a Context,
        topic: Option<CowValue<'a>>,
    ) -> Result<Option<Vec<CowValue<'a>>>> {
        match self {
            Self::Comma(l, r) => {
                let Some(mut vec) = l.eval(ctx, topic.clone())? else {
                    return Ok(None);
                };
                let Some(val) = r.eval(ctx, topic)? else {
                    return Ok(None);
                };
                vec.push(val);
                Ok(Some(vec))
            },
            Self::Arg(e) => e.eval(ctx, topic).map(|v| v.map(|v| vec![v])),
        }
    }
}

#[derive(Debug)]
pub enum Arg<'a> {
    Coerce(Expr<'a>),
    Expr(Expr<'a>),
}

impl<'a, 's> Eval<'a> for Arg<'s> {
    type Output = Option<CowValue<'a>>;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<Option<CowValue<'a>>> {
        assert_no_topic(&topic, &self)?;

        match self {
            Arg::Coerce(e) => Ok(match e.eval(ctx, topic)? {
                Owned(Value::Null) | Borrowed(Value::Null) => None,
                v => Some(v),
            }),
            Arg::Expr(e) => e.eval(ctx, topic).map(Some),
        }
    }
}
