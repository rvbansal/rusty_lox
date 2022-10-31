use super::constants::{MAX_FUNC_ARGS, SUPER_STR, THIS_STR};
use super::grammar::{Expr, ExprType, FuncInfo, Literal, Stmt, StmtType, VariableInfo};
use super::lexer::Lexer;
use super::operator::{InfixOperator, LogicalOperator, Precedence, PrefixOperator};
use super::span::{CodePosition, Span};
use super::token::{SpannedToken, Token};

pub struct Parser<'s> {
    source: &'s str,
    lexer: Lexer<'s>,
    current: SpannedToken,
    previous: SpannedToken,
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken(Token, CodePosition, Token),
    ExpectedExpr(CodePosition, Token),
    ExpectedIdentifier(CodePosition),
    TooManyArgs(CodePosition),
    ExpectedLValue(CodePosition),
}

pub type ParserResult<T> = Result<T, ParserError>;

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        let dummy_token = SpannedToken {
            token: Token::LexerError("<parser token>".to_owned()),
            span: Span::default(),
        };

        let mut parser = Parser {
            source,
            lexer: Lexer::new(source),
            current: dummy_token.clone(),
            previous: dummy_token,
        };

        parser.bump();
        parser
    }

    /// Advances the stream.
    fn bump(&mut self) {
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = self.lexer.next_token();
    }

    /// Checks whether or not the current token matches the given token.
    fn check(&mut self, t: Token) -> bool {
        self.current.token == t
    }

    /// Checks whether or not the current token matches the given token.
    /// If true consume it and return true, else return false.
    fn check_consume(&mut self, t: Token) -> bool {
        if self.check(t) {
            self.bump();
            return true;
        }
        false
    }

    /// Checks whether or not the current token matches the given token.
    /// If true consume it, else return Error.
    fn consume(&mut self, t: Token) -> ParserResult<()> {
        self.bump();

        if self.previous.token == t {
            Ok(())
        } else {
            Err(ParserError::ExpectedToken(
                t,
                self.previous.span.start_pos,
                self.previous.token.clone(),
            ))
        }
    }

    /// Parses program from the top of treating it as a set of statements.
    pub fn parse(mut self) -> Vec<ParserResult<Stmt>> {
        let mut stmts = vec![];

        while !self.check(Token::EndOfFile) {
            let stmt = self.parse_declaration();
            stmts.push(stmt);
        }

        stmts
    }

    /// Handle variable declations separately from non-declaring statements since
    /// they may not be allowed everywhere non-declaring statements are allowed.
    fn parse_declaration(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        match self.current.token {
            Token::Var => {
                self.bump();
                let name = self.parse_identifier()?;
                let expr = if self.check_consume(Token::Equals) {
                    self.parse_expression()?
                } else {
                    to_expr(from_literal(Literal::Nil), Span::default())
                };
                self.consume(Token::Semicolon)?;

                Ok(to_stmt(
                    StmtType::VariableDecl(name, expr),
                    curr_span.extend(self.previous.span),
                ))
            }
            Token::Fun => {
                let func_info = self.parse_func_info(false)?;

                Ok(to_stmt(
                    StmtType::FuncDecl(func_info),
                    curr_span.extend(self.previous.span),
                ))
            }
            Token::Class => self.parse_class_decl(),
            _ => self.parse_statement(),
        }
    }

    /// Parse func info into func info struct.
    fn parse_func_info(&mut self, is_method: bool) -> ParserResult<FuncInfo> {
        if !is_method {
            self.consume(Token::Fun)?;
        }
        let name = self.parse_identifier()?;
        let params = self.parse_func_params()?;
        let body = self.parse_block()?;
        let func_info = FuncInfo::new(name, params, body);

        Ok(func_info)
    }

    /// Parse print and expression statements.
    fn parse_statement(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        match self.current.token {
            Token::Print => {
                self.bump();
                let expr = self.parse_expression()?;
                self.consume(Token::Semicolon)?;

                Ok(to_stmt(
                    StmtType::Print(expr),
                    curr_span.extend(self.previous.span),
                ))
            }
            Token::If => self.parse_if_else(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Return => self.parse_return(),
            Token::LeftBrace => self.parse_block(),
            _ => {
                let expr = self.parse_expression()?;
                self.consume(Token::Semicolon)?;

                Ok(to_stmt(
                    StmtType::Expression(expr),
                    curr_span.extend(self.previous.span),
                ))
            }
        }
    }

    /// Parse return statement.
    fn parse_return(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        self.consume(Token::Return)?;
        let expr = if !self.check(Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume(Token::Semicolon)?;

        Ok(to_stmt(
            StmtType::Return(expr),
            curr_span.extend(self.previous.span),
        ))
    }

    /// Parse function params into a vector of strings.
    fn parse_func_params(&mut self) -> ParserResult<Vec<String>> {
        self.consume(Token::LeftParen)?;

        let mut args = vec![];
        if self.check_consume(Token::RightParen) {
            return Ok(args);
        }
        args.push(self.parse_identifier()?);

        while !self.check_consume(Token::RightParen) {
            self.consume(Token::Comma)?;
            args.push(self.parse_identifier()?);
        }

        if args.len() >= MAX_FUNC_ARGS {
            let curr_span = self.current.span;
            return Err(ParserError::TooManyArgs(curr_span.start_pos));
        }

        Ok(args)
    }

    /// Parse class declaration.
    fn parse_class_decl(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        self.consume(Token::Class)?;
        let name = self.parse_identifier()?;

        let superclass = if self.check_consume(Token::LeftAngle) {
            Some(VariableInfo::new(self.parse_identifier()?))
        } else {
            None
        };

        self.consume(Token::LeftBrace)?;

        let mut methods_info = vec![];
        while !self.check_consume(Token::RightBrace) {
            let method_info = self.parse_func_info(true)?;
            methods_info.push(method_info);
        }

        Ok(to_stmt(
            StmtType::ClassDecl(name, superclass, methods_info),
            curr_span.extend(self.previous.span),
        ))
    }

    /// Parse if else statement.
    fn parse_if_else(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        self.consume(Token::If)?;
        self.consume(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume(Token::RightParen)?;
        let if_body = Box::new(self.parse_statement()?);
        let else_body = if self.check_consume(Token::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Ok(to_stmt(
            StmtType::IfElse(condition, if_body, else_body),
            curr_span.extend(self.previous.span),
        ))
    }

    /// Parse while statement.
    fn parse_while(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        self.consume(Token::While)?;
        self.consume(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume(Token::RightParen)?;
        let body = self.parse_statement()?;

        Ok(to_stmt(
            StmtType::While(condition, Box::new(body)),
            curr_span.extend(self.previous.span),
        ))
    }

    /// Parse for loop.
    fn parse_for(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        self.consume(Token::For)?;
        self.consume(Token::LeftParen)?;

        // Get initializer. It can be empty.
        let init_stmt = if self.check_consume(Token::Semicolon) {
            None
        } else if self.check(Token::Var) {
            Some(self.parse_declaration()?)
        } else {
            let curr_span = self.current.span;
            let expr = self.parse_expression()?;
            self.consume(Token::Semicolon)?;
            Some(to_stmt(
                StmtType::Expression(expr),
                curr_span.extend(self.previous.span),
            ))
        };

        // Get condition expression. It can be empty.
        let condition = if self.check_consume(Token::Semicolon) {
            to_expr(from_literal(Literal::Boolean(true)), Span::default())
        } else {
            self.parse_expression()?
        };
        self.consume(Token::Semicolon)?;

        // Get increment expression. It can be empty.
        let increment = if self.check_consume(Token::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume(Token::RightParen)?;

        // Get body.
        let mut body = self.parse_statement()?;

        // Treat the increment as part of the body.
        if let Some(increment) = increment {
            let increment_smt = to_stmt(StmtType::Expression(increment), Span::default());
            body = to_stmt(StmtType::Block(vec![body, increment_smt]), Span::default())
        }

        // Convert body to a while loop.
        body = to_stmt(StmtType::While(condition, Box::new(body)), Span::default());

        // Add the initial condition if it exists prior to the body.
        if let Some(init_stmt) = init_stmt {
            body = to_stmt(
                StmtType::Block(vec![init_stmt, body]),
                curr_span.extend(self.previous.span),
            );
        }

        Ok(body)
    }

    /// Parse a block of statements.
    fn parse_block(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        let mut stmts = vec![];
        self.consume(Token::LeftBrace)?;
        while !self.check_consume(Token::RightBrace) {
            stmts.push(self.parse_declaration()?);
        }

        Ok(to_stmt(
            StmtType::Block(stmts),
            curr_span.extend(self.previous.span),
        ))
    }

    /// Parse an identifier.
    fn parse_identifier(&mut self) -> ParserResult<String> {
        self.bump();
        match &self.previous.token {
            Token::Identifier(name) => Ok(name.to_owned()),
            _ => Err(ParserError::ExpectedIdentifier(
                self.previous.span.start_pos,
            )),
        }
    }

    /// Parse function args.
    fn parse_fn_args(&mut self) -> ParserResult<Vec<Expr>> {
        self.consume(Token::LeftParen)?;

        let mut args = vec![];
        if self.check_consume(Token::RightParen) {
            return Ok(args);
        }

        args.push(self.parse_expression()?);
        while !self.check_consume(Token::RightParen) {
            self.consume(Token::Comma)?;
            args.push(self.parse_expression()?);
        }

        if args.len() >= MAX_FUNC_ARGS {
            let curr_span = self.current.span;
            return Err(ParserError::TooManyArgs(curr_span.start_pos));
        }

        Ok(args)
    }

    /// Parse expression with precedence.
    pub fn parse_expression(&mut self) -> ParserResult<Expr> {
        self.run_pratt_parse_algo(Precedence::Lowest)
    }

    /// Pratt parsing algo.
    pub fn run_pratt_parse_algo(&mut self, min_precedence: Precedence) -> ParserResult<Expr> {
        let curr_span = self.current.span;

        // Start by parsing literals or prefixes.
        let mut lhs = match PrefixOperator::from_token(&self.current.token) {
            Some(op) => {
                self.bump();
                let expr = self.run_pratt_parse_algo(op.precedence())?;
                to_expr(
                    ExprType::Prefix(op, Box::new(expr)),
                    curr_span.extend(self.previous.span),
                )
            }
            None => self.parse_primary()?,
        };

        // Recursively handle any infixes with same or higher precedence.
        loop {
            // Standard infix
            if let Some(op) = InfixOperator::from_token(&self.current.token) {
                if op.precedence() <= min_precedence {
                    break;
                }

                self.bump();
                let rhs = self.run_pratt_parse_algo(op.precedence())?;
                let new_span = lhs.span.extend(rhs.span);
                lhs = to_expr(ExprType::Infix(op, Box::new(lhs), Box::new(rhs)), new_span);
                continue;
            }

            // Logical
            if let Some(op) = LogicalOperator::from_token(&self.current.token) {
                if op.precedence() <= min_precedence {
                    break;
                }

                self.bump();
                let rhs = self.run_pratt_parse_algo(op.precedence())?;
                let new_span = lhs.span.extend(rhs.span);
                lhs = to_expr(
                    ExprType::Logical(op, Box::new(lhs), Box::new(rhs)),
                    new_span,
                );
                continue;
            }

            // Assignment
            if let Token::Equals = self.current.token {
                if Precedence::Assignment < min_precedence {
                    break;
                }

                self.bump();
                let rhs = self.run_pratt_parse_algo(Precedence::Assignment)?;
                let new_span = lhs.span.extend(rhs.span);
                let rhs_box = Box::new(rhs);
                let new_expr = match lhs.expr {
                    ExprType::Variable(var_info) => {
                        ExprType::Assignment(VariableInfo::new(var_info.name), rhs_box)
                    }
                    ExprType::Get(expr, property) => ExprType::Set(expr, property, rhs_box),
                    _ => return Err(ParserError::ExpectedLValue(curr_span.start_pos)),
                };
                lhs = to_expr(new_expr, new_span);
                continue;
            }

            // Dot
            if let Token::Dot = self.current.token {
                if Precedence::Property < min_precedence {
                    break;
                }

                self.bump();
                let rhs = self.parse_identifier()?;
                let new_span = lhs.span.extend(self.previous.span);
                lhs = to_expr(ExprType::Get(Box::new(lhs), rhs), new_span);
                continue;
            }

            // Function call
            if let Token::LeftParen = self.current.token {
                if Precedence::Call < min_precedence {
                    break;
                }

                let args = self.parse_fn_args()?;
                let new_span = lhs.span.extend(self.previous.span);
                lhs = to_expr(ExprType::Call(Box::new(lhs), args), new_span);
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    /// Parse primary token.
    fn parse_primary(&mut self) -> ParserResult<Expr> {
        self.bump();
        let curr_span = self.previous.span;

        let expr = match &self.previous.token {
            Token::Number(n) => from_literal(Literal::Number(*n)),
            Token::True => from_literal(Literal::Boolean(true)),
            Token::False => from_literal(Literal::Boolean(false)),
            Token::String(s) => from_literal(Literal::Str(s.to_owned())),
            Token::Nil => from_literal(Literal::Nil),
            Token::Identifier(name) => ExprType::Variable(VariableInfo::new(name.to_owned())),
            Token::This => ExprType::This(VariableInfo::new(THIS_STR.to_owned())),
            Token::Super => {
                let var = VariableInfo::new(SUPER_STR.to_owned());
                self.consume(Token::Dot)?;
                let method_name = self.parse_identifier()?;
                ExprType::Super(var, method_name)
            }
            Token::LeftParen => {
                let sub_expr = self.parse_expression()?;
                self.consume(Token::RightParen)?;
                return Ok(sub_expr);
            }
            t => return Err(ParserError::ExpectedExpr(curr_span.start_pos, t.clone())),
        };

        Ok(to_expr(expr, curr_span.extend(self.previous.span)))
    }
}

fn from_literal(l: Literal) -> ExprType {
    ExprType::Literal(l)
}

fn to_stmt(stmt: StmtType, span: Span) -> Stmt {
    Stmt::new(stmt, span)
}

fn to_expr(expr: ExprType, span: Span) -> Expr {
    Expr::new(expr, span)
}
