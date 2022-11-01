use super::span::Span;
use super::token::Token;

use std::fmt;

pub const MAX_ARGS: usize = 256;

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken(Token, Span, Token),
    ExpectedExpr(Span, Token),
    ExpectedIdentifier(Span),
    TooManyArgs(Span),
    ExpectedLValue(Span),
    IllegalToken(Span, String),
}

pub type ParserResult<T> = Result<T, ParserError>;

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::ExpectedToken(expected, span, got) => {
                write!(
                    f,
                    "Expected {:?} on line {}, but instead got {:?}.",
                    expected, span.start_pos.line_no, got
                )
            }
            ParserError::ExpectedExpr(span, got) => {
                write!(
                    f,
                    "Expected expression on line {}, but instead got {:?}.",
                    span.start_pos.line_no, got
                )
            }
            ParserError::ExpectedIdentifier(span) => {
                write!(f, "Expected identifier on line {}.", span.start_pos.line_no)
            }
            ParserError::ExpectedLValue(span) => {
                write!(f, "Expected lvalue on line {}.", span.start_pos.line_no)
            }
            ParserError::TooManyArgs(span) => {
                write!(
                    f,
                    "Too many arguments for function on line {}. Lox only supports {} arguments.",
                    span.start_pos.line_no, MAX_ARGS
                )
            }
            ParserError::IllegalToken(span, string) => {
                write!(
                    f,
                    "Illegal token {} on line {}.",
                    string, span.start_pos.line_no
                )
            }
        }
    }
}
