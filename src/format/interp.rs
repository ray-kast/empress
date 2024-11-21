use std::{
    borrow::Cow,
    fmt::{self, Debug, Write},
};

use serde_json::Map;

use super::{ffi, functions::Functions};

pub(super) fn json(value: &Value) -> String {
    serde_json::to_string(&value).unwrap_or_else(|_| "<error>".into())
}

pub(super) fn assert_no_topic<D: Debug>(topic: &Option<CowValue>, d: &D) -> Result<()> {
    match topic {
        Some(_) => Err(Error::ExtraTopic(format!("{d:?}"))),
        None => Ok(()),
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
    #[error("Unexpected pipe input when evaluating {0}")]
    ExtraTopic(String),
    #[error("Error executing function {0:?}")]
    Ffi(String, #[source] ffi::Error),
    #[error("Value {} has no field {1:?}", json(.0))]
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

pub trait Eval<'a> {
    type Output: 'a;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<Self::Output>;
}

pub trait StreamString<'a>: Stream<'a> {
    fn stream_string(self, ctx: Self::Context) -> Result<String, Self::Error>;
}

impl<'a, T: Stream<'a>> StreamString<'a> for T {
    fn stream_string(self, ctx: Self::Context) -> Result<String, Self::Error> {
        let mut s = String::new();
        self.stream(ctx, &mut s)?;
        Ok(s)
    }
}

pub trait Stream<'a> {
    type Context: 'a;
    type Error: From<StreamError>;

    fn stream(self, ctx: Self::Context, out: impl Write) -> Result<(), Self::Error>;
}

impl Stream<'static> for &Value {
    type Context = ();
    type Error = StreamError;

    fn stream(self, (): (), mut out: impl Write) -> Result<(), StreamError> {
        match self {
            Value::Null => Ok(()),
            Value::Number(n) => out.write_fmt(format_args!("{n}")).map_err(Into::into),
            Value::String(s) => out.write_str(s).map_err(Into::into),
            Value::Bool(_) | Value::Array(_) | Value::Object(_) => {
                Err(StreamError::Unprintable(self.clone()))
            },
        }
    }
}

pub trait StreamAll<'a>: IntoIterator {
    type Context: 'a;
    type Error: From<StreamError>;

    fn stream_all(self, ctx: Self::Context, out: impl Write) -> Result<(), Self::Error>;
}

impl<'a, T: Stream<'a>, I: IntoIterator<Item = T>> StreamAll<'a> for I
where T::Context: Clone
{
    type Context = T::Context;
    type Error = T::Error;

    fn stream_all(self, ctx: T::Context, mut out: impl Write) -> Result<(), T::Error> {
        self.into_iter()
            .try_for_each(|s| s.stream(ctx.clone(), &mut out))
    }
}
