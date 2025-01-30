use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
    fmt::{self, Debug},
};

use serde_json::Map;

use super::{ffi, functions::Functions};

pub(super) fn json(value: &Value) -> String {
    serde_json::to_string(&value).unwrap_or_else(|_| "<error>".into())
}

#[expect(
    clippy::ref_option,
    reason = "This is a domain-specific helper function"
)]
pub(super) fn assert_no_topic<D: Debug>(topic: &Option<CowValue>, d: &D) -> Result<()> {
    match topic {
        Some(_) => Err(Error::ExtraTopic(format!("{d:?}"))),
        None => Ok(()),
    }
}

#[expect(
    clippy::ref_option,
    reason = "This is a domain-specific helper function"
)]
pub(super) fn assert_topic<D: Debug>(topic: &Option<CowValue<'_>>, d: &D) -> Result<()> {
    match topic {
        Some(_) => Ok(()),
        None => Err(Error::NoTopic(format!("{d:?}"))),
    }
}

#[inline]
pub(super) fn is_null_like(val: &Value) -> bool {
    match val {
        Value::Null => true,
        Value::String(s) if s.is_empty() => true,
        _ => false,
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error writing format output")]
    Stream(#[source] StreamError),
    #[error("No pipe input available when evaluating {0}")]
    NoTopic(String),
    #[error("Unexpected pipe input when evaluating {0}")]
    ExtraTopic(String),
    #[error("Error executing function {0:?}")]
    Ffi(String, #[source] ffi::Error),
    #[error("Variable name {0:?} already exists")]
    Shadow(String),
    #[error("Value {} must be a boolean", json(.0))]
    BadCondition(Value),
    #[error("Values {} and {} cannot be compared", json(.0), json(.1))]
    BadCompare(Value, Value),
    #[error("Value {val} has no field {1:?}", val = json(.0))]
    BadPath(Value, String),
    #[error("Value {} has no index {}", json(.0), json(.1))]
    BadIndex(Value, Value),
    #[error("No value found named {0:?}")]
    NoValue(String),
    #[error("No function found named {0:?}")]
    NoFunction(String),
}

#[derive(Debug, thiserror::Error)]
pub enum StreamError {
    #[error("An I/O error occurred")]
    Io(#[from] fmt::Error),
    #[error("Cannot format {} as a string", json(.0))]
    Unprintable(Value),
}

impl<T: Into<StreamError>> From<T> for Error {
    fn from(err: T) -> Self { Self::Stream(err.into()) }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
pub use serde_json::Value;
pub type CowValue<'a> = Cow<'a, Value>;

#[derive(Default)]
pub struct State<'a> {
    pub locals: HashMap<&'a str, CowValue<'a>>,
}

pub struct Context {
    pub values: Map<String, Value>,
    pub functions: Functions,
}

impl Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Context")
            .field("values", &self.values)
            .finish_non_exhaustive()
    }
}

pub trait EvalMut {
    type Output<'a>
    where Self: 'a;

    fn eval_mut<'a>(
        &'a self,
        ctx: &'a Context,
        state: &mut State<'a>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output<'a>>;
}

pub trait Eval {
    type Output<'a>
    where Self: 'a;

    fn eval<'a>(
        &'a self,
        ctx: &'a Context,
        state: &State<'a>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output<'a>>;
}

impl<T: Eval> EvalMut for T {
    type Output<'a>
        = <T as Eval>::Output<'a>
    where Self: 'a;

    #[inline]
    fn eval_mut<'a>(
        &'a self,
        ctx: &'a Context,
        state: &mut State<'a>,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output<'a>> {
        self.eval(ctx, state, topic)
    }
}

pub fn write_value<V: Borrow<Value>, W: fmt::Write>(
    value: V,
    mut out: W,
) -> Result<(), StreamError> {
    let value = value.borrow();
    match value {
        Value::Null => Ok(()),
        Value::Number(n) => out.write_fmt(format_args!("{n}")).map_err(Into::into),
        Value::String(s) => out.write_str(s).map_err(Into::into),
        Value::Bool(_) | Value::Array(_) | Value::Object(_) => {
            Err(StreamError::Unprintable(value.clone()))
        },
    }
}

pub fn stringify<V: Borrow<Value>>(value: V) -> Result<String, StreamError> {
    let mut s = String::new();
    write_value(value, &mut s).map(|()| s)
}
