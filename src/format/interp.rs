use std::{borrow::Cow, collections::HashMap, fmt::Write};

use serde_json::Map;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Formatting output failed")]
    Io(#[from] std::fmt::Error),
    #[error("Cannot format {0:?} as a string")]
    Unprintable(Value),
    #[error("Type error: expected {1}, got {0:?}")]
    TypeError(Value, &'static str),
    #[error("Value {0:?} has no field {1:?}")]
    BadPath(Value, String),
    #[error("No value found named {0:?}")]
    NoValue(String),
    #[error("No function found named {0:?}")]
    NoFunction(String),
}

pub type Result<T> = std::result::Result<T, Error>;
pub use serde_json::Value;
pub type CowValue<'a> = Cow<'a, Value>;
pub type Function = for<'a> fn(Option<CowValue<'a>>, Vec<CowValue<'a>>) -> Result<CowValue<'a>>;

pub struct Context {
    pub values: Map<String, Value>,
    pub functions: HashMap<String, Function>,
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
