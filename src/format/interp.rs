use std::{
    borrow::{Borrow, Cow},
    fmt::{self, Debug, Write},
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

pub trait Eval {
    type Output<'a>
    where Self: 'a;

    fn eval<'a>(
        &'a self,
        ctx: &'a Context,
        topic: Option<CowValue<'a>>,
    ) -> Result<Self::Output<'a>>;
}

pub trait StreamString: Stream {
    fn stream_string(&self, ctx: Self::Context<'_>) -> Result<String, Self::Error>;
}

impl<T: Stream> StreamString for T {
    fn stream_string(&self, ctx: Self::Context<'_>) -> Result<String, Self::Error> {
        let mut s = String::new();
        self.stream(ctx, &mut s)?;
        Ok(s)
    }
}

pub trait Stream {
    type Context<'a>: Copy;
    type Error: From<StreamError>;

    fn stream<W: Write>(&self, ctx: Self::Context<'_>, out: W) -> Result<(), Self::Error>;
}

impl Stream for Value {
    type Context<'a> = ();
    type Error = StreamError;

    fn stream<W: Write>(&self, (): (), mut out: W) -> Result<(), StreamError> {
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

pub trait StreamAll: IntoIterator {
    type Context<'a>;
    type Error: From<StreamError>;

    fn stream_all<W: Write>(self, ctx: Self::Context<'_>, out: W) -> Result<(), Self::Error>;
}

impl<'b, T: Stream + 'b, I: IntoIterator<Item = &'b T>> StreamAll for I {
    type Context<'a> = T::Context<'a>;
    type Error = T::Error;

    fn stream_all<W: Write>(self, ctx: T::Context<'_>, mut out: W) -> Result<(), T::Error> {
        self.into_iter()
            .try_for_each(|s| s.borrow().stream(ctx, &mut out))
    }
}
