use nom::error::Error as NomError;

mod ast;
mod ffi;
mod functions;
mod interp;
mod lexer;

mod __parser {
    #![allow(clippy::all, warnings)]

    lalrpop_util::lalrpop_mod!(pub parser, "/format/parser.rs");
}

use __parser::parser;

type ParseError<T> = lalrpop_util::ParseError<lexer::Pos, T, &'static str>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error parsing format string")]
    Lex(#[from] NomError<String>),
    #[error("Error parsing format string")]
    Parse(#[from] ParseError<String>),
    #[error("Error loading context into the interpreter")]
    Values(#[from] anyhow::Error),
    #[error("Error evaluating format string")]
    Interpret(#[from] interp::Error),
}

impl From<NomError<&str>> for Error {
    fn from(NomError { input, code }: NomError<&str>) -> Self {
        Self::Lex(NomError {
            input: input.into(),
            code,
        })
    }
}

impl From<ParseError<lexer::Token<'_>>> for Error {
    fn from(e: ParseError<lexer::Token>) -> Self { Self::Parse(e.map_token(|t| format!("{t:?}"))) }
}

pub fn eval(fmt: impl AsRef<str>, values: impl serde::Serialize) -> Result<String, Error> {
    use interp::StreamAll;

    let toks = lexer::scan(fmt.as_ref());
    log::trace!("{toks:?}");
    let toks = toks?;

    let ast = parser::FormatParser::new().parse(toks);
    log::trace!("{ast:?}");
    let ast = ast?;

    let values = match serde_json::to_value(values) {
        Ok(serde_json::Value::Object(m)) => Ok(m),
        Ok(v) => Err(Error::Values(anyhow::anyhow!(
            "Value provided was not a JSON map ({v:?})",
        ))),
        Err(e) => Err(Error::Values(e.into())),
    }?;

    let mut out = String::new();
    ast.stream_all(
        &interp::Context {
            values,
            functions: functions::all(),
        },
        &mut out,
    )?;

    Ok(out)
}
