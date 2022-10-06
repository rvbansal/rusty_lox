use crate::treewalk::ast;
use crate::treewalk::operator::{InfixOperator, Precedence, PrefixOperator};
use crate::treewalk::span::CodePosition;
use crate::treewalk::token::{SpannedToken, Token};
use std::iter::Peekable;

pub struct Parser<T>
where
    T: Iterator<Item = SpannedToken>,
{
    tokens: Peekable<T>,
}

#[derive(Debug)]
pub enum ParserError {
    BeyondEndOfFile,
    ExpectedTokenAt(Token, CodePosition, Token),
    ExpectedExprAt(CodePosition, Token),
}

pub type ParserResult<T> = Result<T, ParserError>;

impl<T> Parser<T>
where
    T: Iterator<Item = SpannedToken>,
{
    pub fn new(tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    /// Returns current token.
    fn peek_token(&mut self) -> ParserResult<&SpannedToken> {
        match self.tokens.peek() {
            Some(t) => Ok(t),
            None => Err(ParserError::BeyondEndOfFile),
        }
    }

    /// Returns current token and advances stream.
    fn take_token(&mut self) -> ParserResult<SpannedToken> {
        match self.tokens.next() {
            Some(t) => Ok(t),
            None => Err(ParserError::BeyondEndOfFile),
        }
    }

    /// Advances the stream, erroring if we're at end of file.
    fn bump(&mut self) -> ParserResult<()> {
        match self.take_token()?.token {
            Token::EndOfFile => Err(ParserError::BeyondEndOfFile),
            _ => Ok(()),
        }
    }

    /// Checks whether or not the current token matches the given token.
    fn check(&mut self, t: Token) -> ParserResult<bool> {
        return Ok(self.peek_token()?.token == t);
    }

    /// Checks whether or not the current token matches the given token.
    /// If true consume it and return true, else return false.
    fn check_consume(&mut self, t: Token) -> ParserResult<bool> {
        if self.check(t)? == true {
            self.bump()?;
            return Ok(true);
        }
        return Ok(false);
    }

    /// Consumes token, asserting that it equals the expected token.
    fn consume(&mut self, t: Token) -> ParserResult<()> {
        let (token, span) = self.take_token()?.split();

        if token == t {
            Ok(())
        } else {
            Err(ParserError::ExpectedTokenAt(t, span.start_pos, token))
        }
    }

    pub fn parse_expression(&mut self) -> ParserResult<ast::Expr> {
        self.parse(Precedence::Lowest)
    }

    pub fn parse(&mut self, min_precedence: Precedence) -> ParserResult<ast::Expr> {
        let (token, span) = self.take_token()?.split();
        let mut lhs = match token {
            // Literals
            Token::Number(n) => ast::Expr::NumberLiteral(n),
            Token::True => ast::Expr::BooleanLiteral(true),
            Token::False => ast::Expr::BooleanLiteral(false),
            Token::String(s) => ast::Expr::StringLiteral(s),
            Token::Nil => ast::Expr::NilLiteral,

            // Parentheses
            Token::LeftParen => {
                let expr = self.parse_expression()?;
                self.consume(Token::RightParen)?;
                expr
            }

            // Prefix operator
            t => match PrefixOperator::from_token(&t) {
                Some(op) => {
                    let expr = self.parse(op.precedence())?;
                    ast::Expr::Prefix(op, Box::new(expr))
                }
                None => return Err(ParserError::ExpectedExprAt(span.start_pos, t)),
            },
        };

        // Handle infix operators.
        loop {
            let token = &self.peek_token()?.token;
            if let Some(op) = InfixOperator::from_token(token) {
                if op.precedence() <= min_precedence {
                    break;
                }

                self.bump()?;
                let rhs = self.parse(op.precedence())?;
                lhs = ast::Expr::Infix(op, Box::new(lhs), Box::new(rhs));
                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::treewalk::ast::Expr;
    use crate::treewalk::token::SpannedToken;
    use crate::treewalk::Lexer;

    fn get_lexed_source(source: &str) -> Vec<SpannedToken> {
        let lexer = Lexer::new(source);
        lexer.iter().map(|r| r.unwrap()).collect()
    }

    #[test]
    fn test_parser() {
        fn test_parse_func(source: &str) -> Expr {
            let mut parser = Parser::new(get_lexed_source(source).into_iter());
            parser.parse_expression().unwrap()
        }

        assert_eq!(
            test_parse_func("99 + 49"),
            Expr::Infix(
                InfixOperator::Add,
                Box::new(Expr::NumberLiteral(99.0)),
                Box::new(Expr::NumberLiteral(49.0)),
            )
        );

        assert_eq!(
            test_parse_func("3 + 99 * 20 - 5"),
            Expr::Infix(
                InfixOperator::Subtract,
                Box::new(Expr::Infix(
                    InfixOperator::Add,
                    Box::new(Expr::NumberLiteral(3.0)),
                    Box::new(Expr::Infix(
                        InfixOperator::Multiply,
                        Box::new(Expr::NumberLiteral(99.0)),
                        Box::new(Expr::NumberLiteral(20.0)),
                    ))
                )),
                Box::new(Expr::NumberLiteral(5.0))
            )
        );

        assert_eq!(
            test_parse_func("-3 * (110 + 220)"),
            Expr::Infix(
                InfixOperator::Multiply,
                Box::new(Expr::Prefix(
                    PrefixOperator::Negate,
                    Box::new(Expr::NumberLiteral(3.0)),
                )),
                Box::new(Expr::Infix(
                    InfixOperator::Add,
                    Box::new(Expr::NumberLiteral(110.0)),
                    Box::new(Expr::NumberLiteral(220.0)),
                ))
            )
        );
    }
}
