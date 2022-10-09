use crate::treewalk::ast::{Expr, Stmt};
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
    ExpectedIdentifierAt(CodePosition),
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

    /// Parses program from the top of treating it as a set of statements.
    pub fn parse(mut self) -> Vec<ParserResult<Stmt>> {
        let mut stmts = vec![];

        while !self.check(Token::EndOfFile).expect("QQ") {
            let stmt = self.parse_declaration();
            stmts.push(stmt);
        }

        return stmts;
    }

    /// Handle variable declations separately from non-declaring statements
    /// since they may not be allowed everywhere non-declaring statements are allowed.
    fn parse_declaration(&mut self) -> ParserResult<Stmt> {
        let (token, _) = self.peek_token()?.split_ref();

        match token {
            Token::Var => {
                self.bump()?;
                let name = self.parse_identifier()?;
                let expr = if self.check_consume(Token::Equals)? {
                    self.parse_expression()?
                } else {
                    Expr::NilLiteral
                };
                self.consume(Token::Semicolon)?;

                Ok(Stmt::VariableDecl(name, expr))
            }
            _ => self.parse_statement(),
        }
    }

    /// Parse print and expression statements.
    fn parse_statement(&mut self) -> ParserResult<Stmt> {
        let (token, _) = self.peek_token()?.split_ref();

        match token {
            Token::Print => {
                self.bump()?;
                let expr = self.parse_expression()?;
                self.check_consume(Token::Semicolon)?;
                Ok(Stmt::Print(expr))
            }
            Token::LeftBrace => self.parse_block(),
            _ => {
                let expr = self.parse_expression()?;
                self.check_consume(Token::Semicolon)?;
                Ok(Stmt::Expression(expr))
            }
        }
    }

    /// Parse a block of statements.
    fn parse_block(&mut self) -> ParserResult<Stmt> {
        let mut stmts = vec![];

        self.consume(Token::LeftBrace)?;
        while !self.check_consume(Token::RightBrace)? {
            stmts.push(self.parse_declaration()?);
        }

        Ok(Stmt::Block(stmts))
    }

    fn parse_identifier(&mut self) -> ParserResult<String> {
        let (token, span) = self.take_token()?.split();
        match token {
            Token::Identifier(name) => Ok(name.clone()),
            _ => Err(ParserError::ExpectedIdentifierAt(span.start_pos)),
        }
    }

    pub fn parse_expression(&mut self) -> ParserResult<Expr> {
        self.run_pratt_parse_algo(Precedence::Lowest)
    }

    pub fn run_pratt_parse_algo(&mut self, min_precedence: Precedence) -> ParserResult<Expr> {
        let (token, span) = self.take_token()?.split();

        // Start by parsing literals or prefixes.
        let mut lhs = match token {
            // Literals
            Token::Number(n) => Expr::NumberLiteral(n),
            Token::True => Expr::BooleanLiteral(true),
            Token::False => Expr::BooleanLiteral(false),
            Token::String(s) => Expr::StringLiteral(s),
            Token::Nil => Expr::NilLiteral,
            Token::Identifier(s) => Expr::Variable(s),

            // Parentheses
            Token::LeftParen => {
                let expr = self.parse_expression()?;
                self.consume(Token::RightParen)?;
                expr
            }

            // Prefix operator
            t => match PrefixOperator::from_token(&t) {
                Some(op) => {
                    let expr = self.run_pratt_parse_algo(op.precedence())?;
                    Expr::Prefix(op, Box::new(expr))
                }
                None => return Err(ParserError::ExpectedExprAt(span.start_pos, t)),
            },
        };

        // Recursively handle any infixes with same or higher precedence.
        loop {
            let (token, span) = self.peek_token()?.split_ref();
            let span = span.to_owned();

            // Standard infix
            if let Some(op) = InfixOperator::from_token(token) {
                if op.precedence() <= min_precedence {
                    break;
                }
                self.bump()?;
                let rhs = self.run_pratt_parse_algo(op.precedence())?;
                lhs = Expr::Infix(op, Box::new(lhs), Box::new(rhs));
                continue;
            }

            // Assignment
            if let Token::Equals = token {
                if Precedence::Assignment < min_precedence {
                    break;
                }
                self.bump()?;
                // The lhs must he a variable identifier.
                let name = match lhs {
                    Expr::Variable(name) => name,
                    _ => return Err(ParserError::ExpectedIdentifierAt(span.start_pos)),
                };
                let rhs = self.run_pratt_parse_algo(Precedence::Assignment)?;
                lhs = Expr::Assignment(name, Box::new(rhs));
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
