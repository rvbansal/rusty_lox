use super::span::Span;
use super::token::Token;

pub const MAX_ARGS: usize = 255;

#[derive(Debug)]
pub enum Item {
    VariableDecl,
    FunctionBody,
    Expression,
    ClassBody,
    PrintValue,
    ReturnValue,
    FunctionName,
    If,
    While,
    For,
    Condition,
    ForClause,
}

#[derive(Debug)]
pub struct ParserError {
    pub span: Span,
    pub error: ParserErrorType,
}

#[derive(Debug)]
pub enum ParserErrorType {
    ExpectedExpr(Token),
    ExpectedIdentifier,
    ExpectedPropertyName,
    ExpectedCommaBetween,
    TooManyArgs,
    TooManyParams,
    ExpectedLValue,
    IllegalToken(String),
    UnclosedBrace,
    ExpectedBefore(&'static str, Item),
    ExpectedAfter(&'static str, Item),
    ExpectedSuperDot,
    ExpectedSuperMethod,
    ExpectedSuperclassName,
}

pub type ParserResult<T> = Result<T, ParserError>;

impl ParserError {
    pub fn render(&self, source: &str) -> String {
        let span = &self.span;

        match &self.error {
            ParserErrorType::ExpectedExpr(_) => {
                format!("{}: Expect expression.", get_error_prefix(span, source),)
            }
            ParserErrorType::ExpectedIdentifier => {
                format!("{}: Expect variable name.", get_error_prefix(span, source),)
            }
            ParserErrorType::ExpectedLValue => {
                format!(
                    "{}: Invalid assignment target.",
                    get_error_prefix(span, source),
                )
            }
            ParserErrorType::ExpectedPropertyName => format!(
                "{}: Expect property name after '.'.",
                get_error_prefix(span, source),
            ),
            ParserErrorType::TooManyArgs => {
                format!(
                    "{}: Can't have more than {} arguments.",
                    get_error_prefix(span, source),
                    MAX_ARGS,
                )
            }
            ParserErrorType::TooManyParams => {
                format!(
                    "{}: Can't have more than {} parameters.",
                    get_error_prefix(span, source),
                    MAX_ARGS,
                )
            }
            ParserErrorType::IllegalToken(msg) => {
                format!("Error: {}.", msg)
            }
            ParserErrorType::UnclosedBrace => {
                format!("{}: Expected }}", get_error_prefix(span, source))
            }
            ParserErrorType::ExpectedSuperDot => format!(
                "{}: Expect '.' after 'super'.",
                get_error_prefix(span, source),
            ),
            ParserErrorType::ExpectedSuperMethod => format!(
                "{}: Expect superclass method name.",
                get_error_prefix(span, source),
            ),
            ParserErrorType::ExpectedSuperclassName => format!(
                "{}: Expect superclass name.",
                get_error_prefix(span, source),
            ),
            ParserErrorType::ExpectedCommaBetween => format!(
                "{}: Expect ',' between elements.",
                get_error_prefix(span, source),
            ),
            ParserErrorType::ExpectedBefore(expected, previous) => format!(
                "{}: Expect '{}' before {}.",
                get_error_prefix(span, source),
                expected,
                previous.as_str(),
            ),
            ParserErrorType::ExpectedAfter(expected, previous) => format!(
                "{}: Expect '{}' after {}.",
                get_error_prefix(span, source),
                expected,
                previous.as_str(),
            ),
        }
    }
}

impl Item {
    pub fn as_str(&self) -> &'static str {
        match self {
            Item::VariableDecl => "variable declaration",
            Item::FunctionBody => "function body",
            Item::Expression => "expression",
            Item::ClassBody => "class body",
            Item::PrintValue => "value",
            Item::ReturnValue => "return value",
            Item::FunctionName => "function name",
            Item::If => "'if'",
            Item::While => "'while'",
            Item::For => "'for'",
            Item::Condition => "condition",
            Item::ForClause => "for clauses",
        }
    }
}

fn get_error_prefix(span: &Span, source: &str) -> String {
    if span.start_pos.byte_pos < source.len() {
        format!("Error at '{}'", span.extract_string(source).unwrap())
    } else {
        String::from("Error at end")
    }
}
