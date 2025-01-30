use std::{
    borrow::Cow::{self, Borrowed, Owned},
    cmp,
    fmt::{self, Write},
};

use super::{
    ffi,
    interp::{
        assert_no_topic, assert_topic, is_null_like, Context, CowValue, Error, Eval, EvalMut,
        Result, State, Value,
    },
};

#[repr(transparent)]
#[derive(Debug)]
pub struct Format<'a>(pub Vec<Segment<'a>>);

impl Format<'_> {
    pub fn eval<W: fmt::Write>(&self, ctx: &Context, out: W) -> Result<()> {
        let mut state = State::new(out);
        self.0.eval_mut(ctx, &mut state, None)
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct FormatExtended<'a>(pub Vec<Command<'a>>);

impl FormatExtended<'_> {
    pub fn eval<W: fmt::Write>(&self, ctx: &Context, out: W) -> Result<()> {
        let mut state = State::new(out);
        self.0.eval_mut(ctx, &mut state, None)
    }
}

#[derive(Debug)]
pub enum Segment<'a> {
    Fragment(Cow<'a, str>),
    Blank,
    If(If<'a, Vec<Segment<'a>>>),
    Let(Let<'a>),
    Expr(Expr<'a>),
}

impl<'a> EvalMut<'a> for Segment<'_> {
    type Output = ();

    fn eval_mut<W: fmt::Write>(
        &'a self,
        ctx: &'a Context,
        state: &mut State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        assert_no_topic(&topic, self)?;

        match self {
            Segment::Fragment(s) => state.out_mut().write_str(s).map_err(Into::into),
            Segment::Blank => Ok(()),
            Segment::If(e) => e.eval_mut(ctx, state, None),
            Segment::Let(l) => l.eval_mut(ctx, state, None),
            Segment::Expr(e) => state
                .write_value(e.eval(ctx, state, None)?)
                .map_err(Into::into),
        }
    }
}

#[derive(Debug)]
pub enum Command<'a> {
    If(If<'a, Vec<Command<'a>>>),
    Let(Let<'a>),
    Put(Vec<Expr<'a>>, Expr<'a>),
}

impl<'a> EvalMut<'a> for Command<'_> {
    type Output = ();

    fn eval_mut<W: Write>(
        &'a self,
        ctx: &'a Context,
        state: &mut State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        assert_no_topic(&topic, self)?;

        match self {
            Command::If(i) => i.eval_mut(ctx, state, topic),
            Command::Let(l) => l.eval_mut(ctx, state, topic),
            Command::Put(exprs, expr) => {
                for expr in exprs {
                    state.write_value(expr.eval(ctx, state, topic.clone())?)?;
                }

                state
                    .write_value(expr.eval(ctx, state, topic)?)
                    .map_err(Into::into)
            },
        }
    }
}

/// Tuple of `(if, elif*, else?)`
#[derive(Debug)]
pub struct If<'a, T>(pub Guard<'a, T>, pub Vec<Guard<'a, T>>, pub Option<T>);

impl<'a, T: EvalMut<'a, Output: Default>> EvalMut<'a> for If<'_, T> {
    type Output = T::Output;

    fn eval_mut<W: Write>(
        &'a self,
        ctx: &'a Context,
        state: &mut State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        let mut res = self.0.eval_mut(ctx, state, topic.clone())?;

        for elif in &self.1 {
            if res.is_some() {
                break;
            }

            res = elif.eval_mut(ctx, state, topic.clone())?;
        }

        if let Some(res) = res {
            Ok(res)
        } else if let Some(otherwise) = &self.2 {
            otherwise.eval_mut(ctx, state, topic)
        } else {
            Ok(Default::default())
        }
    }
}

fn bool(val: CowValue<'_>) -> Result<bool> {
    match val {
        Owned(Value::Bool(b)) | Borrowed(&Value::Bool(b)) => Ok(b),
        v => Err(Error::BadCondition(v.into_owned())),
    }
}

// Tuple of a list of commands to be run only if the attached expression is true
#[derive(Debug)]
pub struct Guard<'a, T>(pub Expr<'a>, pub T);

impl<'a, T: EvalMut<'a>> EvalMut<'a> for Guard<'_, T> {
    type Output = Option<T::Output>;

    fn eval_mut<W: Write>(
        &'a self,
        ctx: &'a Context,
        state: &mut State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        if bool(self.0.eval(ctx, state, None)?)? {
            Ok(Some(self.1.eval_mut(ctx, state, topic)?))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub struct Let<'a>(pub &'a str, pub Expr<'a>);

impl<'a> EvalMut<'a> for Let<'_> {
    type Output = ();

    fn eval_mut<W: Write>(
        &'a self,
        ctx: &'a Context,
        state: &mut State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        use std::collections::hash_map::Entry;

        let val = self.1.eval(ctx, state, topic)?;
        match (ctx.values.contains_key(self.0), state.locals.entry(self.0)) {
            (false, Entry::Vacant(v)) => {
                v.insert(val);
                Ok(())
            },
            (true, _) | (_, Entry::Occupied(_)) => Err(Error::Shadow(self.0.into())),
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct Expr<'a>(pub Box<NullChain<'a>>);

impl<'a> Eval<'a> for Expr<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        self.0.eval(ctx, state, topic)
    }
}

#[derive(Debug)]
pub enum NullChain<'a> {
    Chain(Box<NullChain<'a>>, NullPipeline<'a>),
    Next(NullPipeline<'a>),
}

impl<'a> Eval<'a> for NullChain<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<CowValue<'a>> {
        match self {
            Self::Chain(c, n) => match c.eval(ctx, state, topic.clone())? {
                Borrowed(v) if is_null_like(v) => n.eval(ctx, state, topic),
                Owned(v) if is_null_like(&v) => n.eval(ctx, state, topic),
                v => Ok(v),
            },
            Self::Next(e) => e.eval(ctx, state, topic),
        }
    }
}

#[derive(Debug)]
pub enum NullPipeline<'a> {
    Bang(Box<NullPipeline<'a>>, Pipeline<'a>),
    Next(Pipeline<'a>),
}

impl<'a> Eval<'a> for NullPipeline<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<CowValue<'a>> {
        match self {
            Self::Bang(n, p) => match n.eval(ctx, state, topic)? {
                v @ (Owned(Value::Null) | Borrowed(Value::Null)) => Ok(v),
                v => p.eval(ctx, state, Some(v)),
            },
            Self::Next(e) => e.eval(ctx, state, topic),
        }
    }
}

#[derive(Debug)]
pub enum Pipeline<'a> {
    Pipe(Box<Pipeline<'a>>, Or<'a>),
    Next(Or<'a>),
}

impl<'a> Eval<'a> for Pipeline<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<CowValue<'a>> {
        match self {
            Self::Pipe(p, l) => l.eval(ctx, state, Some(p.eval(ctx, state, topic)?)),
            Self::Next(e) => e.eval(ctx, state, topic),
        }
    }
}

#[derive(Debug)]
pub enum Or<'a> {
    Or(And<'a>, Box<Or<'a>>),
    Next(And<'a>),
}

impl<'a> Eval<'a> for Or<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        match self {
            Self::Or(l, r) => Ok(Owned(Value::Bool(
                bool(l.eval(ctx, state, topic.clone())?)?
                    || bool(r.eval(ctx, state, topic.clone())?)?,
            ))),
            Self::Next(e) => e.eval(ctx, state, topic),
        }
    }
}

#[derive(Debug)]
pub enum And<'a> {
    And(Compare<'a>, Box<And<'a>>),
    Next(Compare<'a>),
}

impl<'a> Eval<'a> for And<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        match self {
            Self::And(l, r) => Ok(Owned(Value::Bool(
                bool(l.eval(ctx, state, topic.clone())?)?
                    && bool(r.eval(ctx, state, topic.clone())?)?,
            ))),
            Self::Next(e) => e.eval(ctx, state, topic),
        }
    }
}

#[derive(Debug)]
pub enum Compare<'a> {
    Eq(Unop<'a>, Unop<'a>),
    Neq(Unop<'a>, Unop<'a>),
    Lt(Unop<'a>, Unop<'a>),
    Gt(Unop<'a>, Unop<'a>),
    Le(Unop<'a>, Unop<'a>),
    Ge(Unop<'a>, Unop<'a>),
    Next(Unop<'a>),
}

fn partial_cmp(l: CowValue<'_>, r: CowValue<'_>) -> Result<Option<cmp::Ordering>> {
    Ok(match (l, r) {
        (
            Owned(Value::Null) | Borrowed(Value::Null),
            Owned(Value::Null) | Borrowed(Value::Null),
        ) => Some(cmp::Ordering::Equal),
        (
            Owned(Value::Bool(l)) | Borrowed(&Value::Bool(l)),
            Owned(Value::Bool(r)) | Borrowed(&Value::Bool(r)),
        ) => Some(l.cmp(&r)),
        (
            Owned(Value::Number(ref l)) | Borrowed(&Value::Number(ref l)),
            Owned(Value::Number(ref r)) | Borrowed(&Value::Number(ref r)),
        ) => {
            if let Some((l, r)) = l.as_i128().and_then(|l| Some((l, r.as_i128()?))) {
                Some(l.cmp(&r))
            } else if let Some((l, r)) = l.as_u128().and_then(|l| Some((l, r.as_u128()?))) {
                Some(l.cmp(&r))
            } else if let Some((l, r)) = l.as_f64().and_then(|l| Some((l, r.as_f64()?))) {
                l.partial_cmp(&r)
            } else {
                None
            }
        },
        (
            Owned(Value::String(ref l)) | Borrowed(&Value::String(ref l)),
            Owned(Value::String(ref r)) | Borrowed(&Value::String(ref r)),
        ) => Some(l.cmp(r)),
        (
            Owned(Value::Array(ref l)) | Borrowed(&Value::Array(ref l)),
            Owned(Value::Array(ref r)) | Borrowed(&Value::Array(ref r)),
        ) => {
            let len = cmp::min(l.len(), r.len());
            let lhs = &l[..len];
            let rhs = &r[..len];

            'chk: {
                for i in 0..len {
                    match partial_cmp(Borrowed(&lhs[i]), Borrowed(&rhs[i]))? {
                        Some(cmp::Ordering::Equal) => (),
                        o => break 'chk o,
                    }
                }

                Some(l.len().cmp(&r.len()))
            }
        },
        (l, r) => return Err(Error::BadCompare(l.into_owned(), r.into_owned())),
    })
}

impl<'a> Eval<'a> for Compare<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        match self {
            Compare::Eq(l, r) => Ok(Owned(Value::Bool(
                l.eval(ctx, state, topic.clone())? == r.eval(ctx, state, topic)?,
            ))),
            Compare::Neq(l, r) => Ok(Owned(Value::Bool(
                l.eval(ctx, state, topic.clone())? != r.eval(ctx, state, topic)?,
            ))),
            Compare::Lt(l, r) => Ok(Owned(Value::Bool(
                partial_cmp(
                    l.eval(ctx, state, topic.clone())?,
                    r.eval(ctx, state, topic)?,
                )?
                .is_some_and(cmp::Ordering::is_lt),
            ))),
            Compare::Gt(l, r) => Ok(Owned(Value::Bool(
                partial_cmp(
                    l.eval(ctx, state, topic.clone())?,
                    r.eval(ctx, state, topic)?,
                )?
                .is_some_and(cmp::Ordering::is_gt),
            ))),
            Compare::Le(l, r) => Ok(Owned(Value::Bool(
                partial_cmp(
                    l.eval(ctx, state, topic.clone())?,
                    r.eval(ctx, state, topic)?,
                )?
                .is_some_and(cmp::Ordering::is_le),
            ))),
            Compare::Ge(l, r) => Ok(Owned(Value::Bool(
                partial_cmp(
                    l.eval(ctx, state, topic.clone())?,
                    r.eval(ctx, state, topic)?,
                )?
                .is_some_and(cmp::Ordering::is_ge),
            ))),
            Self::Next(e) => e.eval(ctx, state, topic),
        }
    }
}

#[derive(Debug)]
pub enum Unop<'a> {
    Not(Box<Unop<'a>>),
    Next(Member<'a>),
}

impl<'a> Eval<'a> for Unop<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output> {
        match self {
            Unop::Not(e) => Ok(Owned(Value::Bool(!bool(e.eval(ctx, state, topic)?)?))),
            Unop::Next(e) => e.eval(ctx, state, topic),
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

fn lens_index<'a, R: Eval<'a, Output = CowValue<'a>>, W>(
    ctx: &'a Context,
    state: &State<'a, W>,
    lhs: CowValue<'a>,
    rhs: &'a R,
) -> Result<CowValue<'a>> {
    fn as_usize(i: &Value) -> Option<usize> { i.as_u64().and_then(|i| i.try_into().ok()) }

    fn array_has_idx(a: &[Value], i: &Value) -> bool { as_usize(i).is_some_and(|i| a.len() > i) }

    fn object_has_idx(m: &serde_json::Map<String, Value>, i: &Value) -> bool {
        i.as_str().is_some_and(|i| m.contains_key(i))
    }

    match (lhs, rhs.eval(ctx, state, None)?) {
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
    Next(Prim<'a>),
}

impl<'a> Eval<'a> for Member<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<CowValue<'a>> {
        match self {
            Self::Dot(l, r) => lens_ident(l.eval(ctx, state, topic)?, r),
            Self::Index(l, r) => lens_index(ctx, state, l.eval(ctx, state, topic)?, r),
            Self::Next(e) => e.eval(ctx, state, topic),
        }
    }
}

fn call<'a, W>(
    ctx: &'a Context,
    state: &State<'a, W>,
    topic: Option<CowValue<'a>>,
    ident: &str,
    args: Option<&'a Args<'a>>,
) -> Result<CowValue<'a>> {
    let Some(args) = args
        .as_ref()
        .map_or_else(|| Ok(Some(vec![])), |a| a.eval(ctx, state, None))?
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

impl<'a> Eval<'a> for Prim<'_> {
    type Output = CowValue<'a>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<CowValue<'a>> {
        if matches!(self, Self::Value(_)) {
            assert_no_topic(&topic, self)?;
        }

        if matches!(self, Self::LensIdent(_) | Self::LensIndex(_)) {
            assert_topic(&topic, self)?;
        }

        #[expect(
            clippy::needless_borrowed_reference,
            reason = "`&ref s` is the least inelegant way I could think of to match &&str as &str"
        )]
        match self {
            Self::Paren(e) => e.eval(ctx, state, topic),
            Self::LensIdent(i) => lens_ident(topic.unwrap_or_else(|| unreachable!()), i),
            Self::LensIndex(e) => {
                lens_index(ctx, state, topic.unwrap_or_else(|| unreachable!()), e)
            },
            Self::Call(&ref i, a) => call(ctx, state, topic, i, a.as_ref()),
            Self::Array(a) => a
                .as_ref()
                .map_or_else(|| Ok(Some(vec![])), |a| a.eval(ctx, state, None))
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
                topic @ Some(_) => call(ctx, state, topic, i, None),
                None => match state
                    .locals
                    .get(i)
                    .cloned()
                    .or_else(|| ctx.values.get(i).map(Borrowed))
                {
                    Some(v) => Ok(v),
                    None => Err(Error::NoValue(i.into())),
                },
            },
            Self::Value(v) => Ok(Borrowed(v)),
        }
    }
}

#[derive(Debug)]
pub struct Args<'a>(pub Vec<Arg<'a>>, pub Arg<'a>);

impl<'a> Eval<'a> for Args<'_> {
    type Output = Option<Vec<CowValue<'a>>>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Option<Vec<CowValue<'a>>>> {
        let mut out = Vec::with_capacity(self.0.len() + 1);

        for expr in &self.0 {
            let Some(val) = expr.eval(ctx, state, topic.clone())? else {
                return Ok(None);
            };
            out.push(val);
        }

        let Some(val) = self.1.eval(ctx, state, topic)? else {
            return Ok(None);
        };
        out.push(val);

        Ok(Some(out))
    }
}

#[derive(Debug)]
pub enum Arg<'a> {
    Coerce(Expr<'a>),
    Expr(Expr<'a>),
}

impl<'a> Eval<'a> for Arg<'_> {
    type Output = Option<CowValue<'a>>;

    fn eval<W>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a, W>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Option<CowValue<'a>>> {
        assert_no_topic(&topic, self)?;

        match self {
            Arg::Coerce(e) => Ok(match e.eval(ctx, state, topic)? {
                Owned(Value::Null) | Borrowed(Value::Null) => None,
                v => Some(v),
            }),
            Arg::Expr(e) => e.eval(ctx, state, topic).map(Some),
        }
    }
}
