use std::{
    borrow::Cow::{Borrowed, Owned},
    fmt::Write,
};

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

impl Stream for Segment<'_> {
    type Context<'a> = &'a Context;
    type Error = Error;

    fn stream<W: Write>(&self, ctx: &Context, mut out: W) -> Result<()> {
        match self {
            Self::Fragment(s) => out.write_str(s).map_err(Into::into),
            Self::Block(e) => e.as_ref().map_or(Ok(()), |e| {
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

impl Eval for Expr<'_> {
    type Output<'a>
        = CowValue<'a>
    where Self: 'a;

    fn eval<'a>(
        &'a self,
        ctx: &'a Context,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output<'a>> {
        self.0.eval(ctx, topic)
    }
}

#[derive(Debug)]
pub enum NullChain<'a> {
    Chain(Box<NullChain<'a>>, NullPipeline<'a>),
    Bang(NullPipeline<'a>),
}

impl Eval for NullChain<'_> {
    type Output<'a>
        = CowValue<'a>
    where Self: 'a;

    fn eval<'a>(&'a self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
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

impl Eval for NullPipeline<'_> {
    type Output<'a>
        = CowValue<'a>
    where Self: 'a;

    fn eval<'a>(&'a self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
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

impl Eval for Pipeline<'_> {
    type Output<'a>
        = CowValue<'a>
    where Self: 'a;

    fn eval<'a>(&'a self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
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

fn lens_index<'a, R: Eval<Output<'a> = CowValue<'a>> + 'a>(
    ctx: &'a Context,
    lhs: CowValue<'a>,
    rhs: &'a R,
) -> Result<CowValue<'a>> {
    fn as_usize(i: &Value) -> Option<usize> { i.as_u64().and_then(|i| i.try_into().ok()) }

    fn array_has_idx(a: &[Value], i: &Value) -> bool { as_usize(i).is_some_and(|i| a.len() > i) }

    fn object_has_idx(m: &serde_json::Map<String, Value>, i: &Value) -> bool {
        i.as_str().is_some_and(|i| m.contains_key(i))
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

impl Eval for Member<'_> {
    type Output<'a>
        = CowValue<'a>
    where Self: 'a;

    fn eval<'a>(&'a self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        match self {
            Self::Dot(l, r) => lens_ident(l.eval(ctx, topic)?, r),
            Self::Index(l, r) => lens_index(ctx, l.eval(ctx, topic)?, r),
            Self::Prim(p) => p.eval(ctx, topic),
        }
    }
}

fn call<'a>(
    ctx: &'a Context,
    topic: Option<CowValue<'a>>,
    ident: &str,
    args: Option<&'a Args<'a>>,
) -> Result<CowValue<'a>> {
    let Some(args) = args
        .as_ref()
        .map_or_else(|| Ok(Some(vec![])), |a| a.eval(ctx, None))?
    else {
        return Ok(Owned(Value::Null));
    };

    ctx.functions
        .get(ident)
        .ok_or_else(|| Error::NoFunction(ident.into()))?(ffi::Input::new(ctx, topic, args))
    .map_err(|e| Error::Ffi(ident.into(), e))
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

impl Eval for Prim<'_> {
    type Output<'a>
        = CowValue<'a>
    where Self: 'a;

    fn eval<'a>(&'a self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<CowValue<'a>> {
        if matches!(self, Self::Value(_)) {
            assert_no_topic(&topic, &self)?;
        }

        if matches!(self, Self::LensIdent(_) | Self::LensIndex(_)) {
            assert_topic(&topic, &self)?;
        }

        #[expect(
            clippy::needless_borrowed_reference,
            reason = "`&ref s` is the least inelegant way I could think of to match &&str as &str"
        )]
        match self {
            Self::Paren(e) => e.eval(ctx, topic),
            Self::LensIdent(i) => lens_ident(topic.unwrap_or_else(|| unreachable!()), i),
            Self::LensIndex(e) => lens_index(ctx, topic.unwrap_or_else(|| unreachable!()), e),
            Self::Call(&ref i, a) => call(ctx, topic, i, a.as_ref()),
            Self::Array(a) => a
                .as_ref()
                .map_or_else(|| Ok(Some(vec![])), |a| a.eval(ctx, None))
                .map(|v| {
                    v.map_or(Owned(Value::Null), |v| {
                        Owned(Value::Array(
                            v.into_iter().map(CowValue::into_owned).collect(),
                        ))
                    })
                }),
            Self::Ident(&ref i) => match topic {
                // A little hacky, but if a topic is present treat idents as a
                // call rather than a value
                topic @ Some(_) => call(ctx, topic, i, None),
                None => match ctx.values.get(i) {
                    Some(v) => Ok(Borrowed(v)),
                    None => Err(Error::NoValue(i.into())),
                },
            },
            Self::Value(v) => Ok(Borrowed(v)),
        }
    }
}

#[derive(Debug)]
pub enum Args<'a> {
    Comma(Box<Args<'a>>, Arg<'a>),
    Arg(Arg<'a>),
}

impl Eval for Args<'_> {
    type Output<'a>
        = Option<Vec<CowValue<'a>>>
    where Self: 'a;

    fn eval<'a>(
        &'a self,
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

impl Eval for Arg<'_> {
    type Output<'a>
        = Option<CowValue<'a>>
    where Self: 'a;

    fn eval<'a>(
        &'a self,
        ctx: &'a Context,
        topic: Option<CowValue<'a>>,
    ) -> Result<Option<CowValue<'a>>> {
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
