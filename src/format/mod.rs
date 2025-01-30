use nom::error::Error as NomError;

mod ast;
mod ffi;
mod functions;
mod interp;
mod lexer;

mod __parser {
    #![expect(clippy::all, warnings, reason = "Generated code")]

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

pub struct Formatter<'a>(FormatterState<'a>);

enum FormatterState<'a> {
    Basic(ast::Format<'a>),
    Extended(ast::FormatExtended<'a>),
}

impl<'a> Formatter<'a> {
    pub fn compile<S: AsRef<str> + ?Sized + 'a>(fmt: &'a S, extended: bool) -> Result<Self, Error> {
        if extended {
            let toks = lexer::scan_extended(fmt.as_ref());
            log::trace!("Extended format string: {toks:?}");

            let ast = parser::FormatExtendedParser::new().parse(toks?);
            log::trace!("{ast:?}");

            Ok(Self(FormatterState::Extended(ast?)))
        } else {
            let toks = lexer::scan(fmt.as_ref());
            log::trace!("Format string: {toks:?}");

            let ast = parser::FormatParser::new().parse(toks?);
            log::trace!("{ast:?}");
            Ok(Self(FormatterState::Basic(ast?)))
        }
    }

    pub fn run<V: serde::Serialize>(&self, values: V) -> Result<String, Error> {
        let values = match serde_json::to_value(values) {
            Ok(serde_json::Value::Object(m)) => Ok(m),
            Ok(v) => Err(Error::Values(anyhow::anyhow!(
                "Value provided was not a JSON map ({v:?})",
            ))),
            Err(e) => Err(Error::Values(e.into())),
        }?;

        let mut out = String::new();
        let ctx = interp::Context {
            values,
            functions: functions::all(),
        };
        match &self.0 {
            FormatterState::Basic(s) => s.eval_print(&ctx, &mut out),
            FormatterState::Extended(c) => c.eval_print(&ctx, &mut out),
        }?;

        Ok(out)
    }
}
