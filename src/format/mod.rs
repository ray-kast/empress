use lalrpop_util::lalrpop_mod;
use nom::error::Error as NomError;

mod ast;
mod interp;
mod lexer;
lalrpop_mod!(parser, "/format/parser.rs");

type ParseError<T> = lalrpop_util::ParseError<lexer::Pos, T, &'static str>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to parse format string: {0}")]
    Lex(#[from] NomError<String>),
    #[error("Failed to parse format string: {0}")]
    Parse(#[from] ParseError<String>),
    #[error("Failed to load context into the interpreter")]
    Values(#[from] anyhow::Error),
    #[error("Failed to evaluate format string: {0}")]
    Interpret(#[from] interp::Error),
}

impl<'a> From<NomError<&'a str>> for Error {
    fn from(NomError { input, code }: NomError<&str>) -> Self {
        Self::Lex(NomError {
            input: input.into(),
            code,
        })
    }
}

impl<'a> From<ParseError<lexer::Token<'a>>> for Error {
    fn from(e: ParseError<lexer::Token>) -> Self {
        Self::Parse(e.map_token(|t| format!("{:?}", t)))
    }
}

pub fn eval(fmt: impl AsRef<str>, values: impl serde::Serialize) -> Result<String, Error> {
    use interp::StreamAll;

    let toks = lexer::scan(fmt.as_ref());
    log::debug!("{:?}", toks);
    let toks = toks?;

    let ast = parser::FormatParser::new().parse(toks);
    log::debug!("{:?}", ast);
    let ast = ast?;

    let values = match serde_json::to_value(values) {
        Ok(serde_json::Value::Object(m)) => Ok(m),
        Ok(v) => Err(Error::Values(anyhow::anyhow!(
            "Value provided was not a JSON map ({:?})",
            v
        ))),
        Err(e) => Err(Error::Values(e.into())),
    }?;

    let mut out = String::new();
    ast.stream_all(
        &interp::Context {
            values,
            functions: vec![].into_iter().collect(),
        },
        &mut out,
    )?;

    Ok(out)
}
