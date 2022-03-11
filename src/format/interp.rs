use std::{
    borrow::Cow,
    fmt::{Debug, Write},
};

use serde_json::Map;

use super::{ffi, functions::Functions};

pub(super) fn json(value: &Value) -> String {
    serde_json::to_string(&value).unwrap_or_else(|_| "<error>".into())
}

pub(super) fn assert_no_topic<D: Debug>(topic: &Option<CowValue>, d: &D) -> Result<()> {
    match topic {
        Some(_) => Err(Error::ExtraTopic(format!("{:?}", d))),
        None => Ok(()),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Formatting output failed")]
    Io(#[from] std::fmt::Error),
    #[error("Cannot format {} as a string", json(.0))]
    Unprintable(Value),
    #[error("Unexpected pipe input when evaluating {0}")]
    ExtraTopic(String),
    #[error("Error executing function {0:?}")]
    Ffi(String, #[source] ffi::Error),
    #[error("Value {} has no field {1:?}", json(.0))]
    BadPath(Value, String),
    #[error("No value found named {0:?}")]
    NoValue(String),
    #[error("No function found named {0:?}")]
    NoFunction(String),
}

pub type Result<T> = std::result::Result<T, Error>;
pub use serde_json::Value;
pub type CowValue<'a> = Cow<'a, Value>;

#[allow(missing_debug_implementations)]
pub struct Context {
    pub values: Map<String, Value>,
    pub functions: Functions,
}

pub trait Eval<'a> {
    type Output: 'a;

    fn eval(self, ctx: &'a Context, topic: Option<CowValue<'a>>) -> Result<Self::Output>;
}

pub trait Stream {
    fn stream(self, ctx: &Context, out: impl Write) -> Result<()>;
}

impl Stream for &Value {
    fn stream(self, _: &Context, mut out: impl Write) -> Result<()> {
        match self {
            Value::Null => Ok(()),
            Value::Number(n) => out.write_fmt(format_args!("{}", n)).map_err(Into::into),
            Value::String(s) => out.write_str(s).map_err(Into::into),
            Value::Bool(_) | Value::Array(_) | Value::Object(_) => {
                Err(Error::Unprintable(self.clone()))
            },
        }
    }
}

pub trait StreamAll: IntoIterator {
    fn stream_all(self, ctx: &Context, out: impl Write) -> Result<()>;
}

impl<T: Stream, I: IntoIterator<Item = T>> StreamAll for I {
    fn stream_all(self, ctx: &Context, mut out: impl Write) -> Result<()> {
        self.into_iter().try_for_each(|s| s.stream(ctx, &mut out))
    }
}
