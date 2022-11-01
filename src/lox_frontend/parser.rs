use super::constants::MAX_FUNC_ARGS;
use super::errors::{Item, ParserError, ParserErrorType, ParserResult};
use super::grammar::Tree;
use super::grammar::{Expr, ExprType, FuncInfo, Literal, Stmt, StmtType};
use super::grammar::{Identifier, PrefixOperator};
use super::lexer::Lexer;
use super::parser_utils::{ParserOperator, Precedence};
use super::span::Span;
use super::token::{SpannedToken, Token};

pub struct Parser<'s> {
    lexer: Lexer<'s>,
    current: SpannedToken,
    previous: SpannedToken,
    errors: Vec<ParserError>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        let dummy_token = SpannedToken {
            token: Token::LexerError("<parser token>".to_owned()),
            span: Span::default(),
        };

        Parser {
            lexer: Lexer::new(source),
            current: dummy_token.clone(),
            previous: dummy_token,
            errors: vec![],
        }
    }

    /// Advances the stream.
    fn bump(&mut self) -> ParserResult<()> {
        std::mem::swap(&mut self.previous, &mut self.current);
        self.current = self.lexer.next_token();

        if let Token::LexerError(e) = &self.current.token {
            Err(ParserError {
                span: self.current.span,
                error: ParserErrorType::IllegalToken(e.clone()),
            })
        } else {
            Ok(())
        }
    }

    /// Checks whether or not the current token matches the given token.
    fn check(&self, t: Token) -> bool {
        self.current.token == t
    }

    /// Checks whether or not the current token matches the given token.
    /// If true consume it and return true, else return false.
    fn check_consume(&mut self, t: Token) -> bool {
        if self.check(t) {
            self.bump().expect("Cannot match error tokens.");
            return true;
        }
        false
    }

    fn expect(&mut self, expected: Token) {
        assert_eq!(self.current.token, expected);
        std::mem::drop(self.bump());
    }

    fn consume(&mut self, t: Token, error: ParserErrorType) -> ParserResult<()> {
        self.bump()?;

        if self.previous.token == t {
            Ok(())
        } else {
            Err(ParserError {
                span: self.previous.span,
                error,
            })
        }
    }

    /// Parses program from the top of treating it as a set of statements.
    pub fn parse(mut self) -> Result<Tree, Vec<ParserError>> {
        if let Err(e) = self.bump() {
            self.emit_error(e);
            self.synchronize();
        };

        let mut stmts = vec![];

        while !self.check(Token::EndOfFile) {
            if let Some(stmt) = self.parse_declaration_with_recovery() {
                stmts.push(stmt);
            }
        }

        if self.errors.is_empty() {
            Ok(Tree { stmts })
        } else {
            Err(self.errors)
        }
    }

    fn synchronize(&mut self) {
        let mut sync_pt = self.previous.token == Token::Semicolon;

        while !sync_pt {
            let current_token = &self.current.token;

            if matches!(current_token, Token::Semicolon | Token::EndOfFile) {
                sync_pt = true;
            }

            std::mem::drop(self.bump());
        }
    }

    fn emit_error(&mut self, error: ParserError) {
        self.errors.push(error);
    }

    /// Handle variable declations separately from non-declaring statements since
    /// they may not be allowed everywhere non-declaring statements are allowed.
    fn parse_declaration_with_recovery(&mut self) -> Option<Stmt> {
        match self.parse_declaration() {
            Ok(stmt) => Some(stmt),
            Err(err) => {
                self.emit_error(err);
                self.synchronize();
                None
            }
        }
    }

    fn parse_declaration(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;

        let stmt_type = match self.current.token {
            Token::Var => self.parse_variable_decl()?,
            Token::Fun => {
                self.bump()?;
                let func_info = self.parse_func_info()?;

                StmtType::FuncDecl(func_info)
            }
            Token::Class => self.parse_class_decl()?,
            _ => return self.parse_statement(),
        };

        Ok(Stmt {
            stmt: stmt_type,
            span: curr_span.extend(self.previous.span),
        })
    }

    /// Parse func info into func info struct.
    fn parse_func_info(&mut self) -> ParserResult<FuncInfo> {
        let curr_span = self.current.span;

        let name = self.parse_identifier(ParserErrorType::ExpectedIdentifier)?;
        let params = self.parse_func_params()?;

        self.consume(
            Token::LeftBrace,
            ParserErrorType::ExpectedBefore("{", Item::FunctionBody),
        )?;
        let stmts = self.parse_block_stmts()?;
        let span = curr_span.extend(self.previous.span);

        let func_info = FuncInfo::new(name, params, stmts, span);

        Ok(func_info)
    }

    /// Parse class declaration.
    fn parse_class_decl(&mut self) -> ParserResult<StmtType> {
        self.expect(Token::Class);
        let name = self.parse_identifier(ParserErrorType::ExpectedIdentifier)?;

        let superclass = if self.check_consume(Token::LeftAngle) {
            let superclass_name = self.parse_identifier(ParserErrorType::ExpectedSuperclassName)?;
            Some(superclass_name)
        } else {
            None
        };

        self.consume(
            Token::LeftBrace,
            ParserErrorType::ExpectedBefore("{", Item::ClassBody),
        )?;
        let mut methods_info = vec![];
        while !self.check_consume(Token::RightBrace) {
            let method_info = self.parse_func_info()?;
            methods_info.push(method_info);
        }

        Ok(StmtType::ClassDecl(name, superclass, methods_info))
    }

    fn parse_variable_decl(&mut self) -> ParserResult<StmtType> {
        self.expect(Token::Var);
        let name = self.parse_identifier(ParserErrorType::ExpectedIdentifier)?;
        let expr = if self.check_consume(Token::Equals) {
            self.parse_expression()?
        } else {
            to_expr(from_literal(Literal::Nil), Span::default())
        };
        self.consume(
            Token::Semicolon,
            ParserErrorType::ExpectedAfter(";", Item::VariableDecl),
        )?;

        Ok(StmtType::VariableDecl(name, expr))
    }

    fn parse_statement(&mut self) -> ParserResult<Stmt> {
        let curr_span = self.current.span;
        let stmt_type = match self.current.token {
            Token::Print => {
                self.bump()?;
                let expr = self.parse_expression()?;
                self.consume(
                    Token::Semicolon,
                    ParserErrorType::ExpectedAfter(";", Item::PrintValue),
                )?;
                StmtType::Print(expr)
            }
            Token::If => self.parse_if_else()?,
            Token::While => self.parse_while()?,
            Token::For => self.parse_for()?,
            Token::Return => self.parse_return()?,
            Token::LeftBrace => {
                self.bump()?;
                let stmts = self.parse_block_stmts()?;
                StmtType::Block(stmts)
            }
            _ => self.parse_expression_statement()?,
        };

        Ok(Stmt {
            stmt: stmt_type,
            span: curr_span.extend(self.previous.span),
        })
    }

    fn parse_expression_statement(&mut self) -> ParserResult<StmtType> {
        let expr = self.parse_expression()?;
        self.consume(
            Token::Semicolon,
            ParserErrorType::ExpectedAfter(";", Item::Expression),
        )?;
        Ok(StmtType::Expression(expr))
    }

    fn parse_if_else(&mut self) -> ParserResult<StmtType> {
        self.expect(Token::If);
        self.consume(
            Token::LeftParen,
            ParserErrorType::ExpectedAfter("(", Item::If),
        )?;
        let condition = self.parse_expression()?;
        self.consume(
            Token::RightParen,
            ParserErrorType::ExpectedAfter(")", Item::Condition),
        )?;

        let if_body = Box::new(self.parse_statement()?);
        let else_body = if self.check_consume(Token::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Ok(StmtType::IfElse(condition, if_body, else_body))
    }

    fn parse_while(&mut self) -> ParserResult<StmtType> {
        self.expect(Token::While);
        self.consume(
            Token::LeftParen,
            ParserErrorType::ExpectedAfter("(", Item::While),
        )?;
        let condition = self.parse_expression()?;
        self.consume(
            Token::RightParen,
            ParserErrorType::ExpectedAfter(")", Item::Condition),
        )?;
        let body = self.parse_statement()?;

        Ok(StmtType::While(condition, Box::new(body)))
    }

    /// Parse for loop.
    fn parse_for(&mut self) -> ParserResult<StmtType> {
        self.expect(Token::For);

        self.consume(
            Token::LeftParen,
            ParserErrorType::ExpectedAfter("(", Item::For),
        )?;

        // Get initializer. It can be empty.
        let init_stmt = if self.check_consume(Token::Semicolon) {
            None
        } else if self.check(Token::Var) {
            let curr_span = self.current.span;
            let stmt = to_stmt(
                self.parse_variable_decl()?,
                curr_span.extend(self.previous.span),
            );
            Some(Box::new(stmt))
        } else {
            let curr_span = self.current.span;
            let stmt = to_stmt(
                self.parse_expression_statement()?,
                curr_span.extend(self.previous.span),
            );
            Some(Box::new(stmt))
        };

        // Get condition expression. It can be empty.
        let condition = if self.check(Token::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.consume(
            Token::Semicolon,
            ParserErrorType::ExpectedAfter(";", Item::Condition),
        )?;

        // Get increment expression. It can be empty.
        let increment = if self.check(Token::RightParen) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.consume(
            Token::RightParen,
            ParserErrorType::ExpectedAfter(")", Item::ForClause),
        )?;

        let body = Box::new(self.parse_statement()?);
        Ok(StmtType::For(init_stmt, condition, increment, body))
    }

    /// Parse return statement.
    fn parse_return(&mut self) -> ParserResult<StmtType> {
        self.expect(Token::Return);
        let expr = if !self.check(Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume(
            Token::Semicolon,
            ParserErrorType::ExpectedAfter(";", Item::ReturnValue),
        )?;
        Ok(StmtType::Return(expr))
    }

    fn parse_block_stmts(&mut self) -> ParserResult<Vec<Stmt>> {
        let mut stmts = vec![];

        while !self.check(Token::RightBrace) && !self.check(Token::EndOfFile) {
            if let Some(stmt) = self.parse_declaration_with_recovery() {
                stmts.push(stmt);
            }
        }

        if self.current.token != Token::EndOfFile {
            self.expect(Token::RightBrace);
            Ok(stmts)
        } else {
            Err(ParserError {
                span: self.current.span,
                error: ParserErrorType::UnclosedBrace,
            })
        }
    }

    /// Parse expression with precedence.
    pub fn parse_expression(&mut self) -> ParserResult<Expr> {
        self.run_pratt_parse_algo(Precedence::Lowest)
    }

    /// Pratt parsing algo.
    pub fn run_pratt_parse_algo(&mut self, min_precedence: Precedence) -> ParserResult<Expr> {
        let prefix_op = match &self.current.token {
            Token::Bang => Some(PrefixOperator::LogicalNot),
            Token::Minus => Some(PrefixOperator::Negate),
            _ => None,
        };

        let mut lhs = match prefix_op {
            Some(op) => {
                let curr_span = self.current.span;
                self.bump()?;
                let expr = self.run_pratt_parse_algo(Precedence::Unary)?;
                to_expr(
                    ExprType::Prefix(op, Box::new(expr)),
                    curr_span.extend(self.previous.span),
                )
            }
            None => self.parse_primary()?,
        };

        while let Some(op) = ParserOperator::from_token(&self.current.token) {
            if !op.is_higher_precedence(min_precedence) {
                break;
            }

            let op_span = self.current.span;

            if op != ParserOperator::Call {
                self.bump()?;
            }

            let precedence = op.precedence();
            let lhs_span = lhs.span;

            let new_lhs = match op {
                ParserOperator::Arithequal(op) => {
                    let rhs = self.run_pratt_parse_algo(precedence)?;
                    ExprType::Infix(op, Box::new(lhs), Box::new(rhs))
                }
                ParserOperator::Logical(op) => {
                    let rhs = self.run_pratt_parse_algo(precedence)?;
                    ExprType::Logical(op, Box::new(lhs), Box::new(rhs))
                }
                ParserOperator::Assignment => {
                    let rhs_box = Box::new(self.run_pratt_parse_algo(precedence)?);
                    match lhs.expr {
                        ExprType::Variable(var) => ExprType::Assignment(var, rhs_box),
                        ExprType::Get(expr, property) => ExprType::Set(expr, property, rhs_box),
                        _ => {
                            return Err(ParserError {
                                span: op_span,
                                error: ParserErrorType::ExpectedLValue,
                            });
                        }
                    }
                }
                ParserOperator::Call => {
                    let arguments = self.parse_func_args()?;
                    ExprType::Call(Box::new(lhs), arguments)
                }
                ParserOperator::Property => {
                    let rhs = self.parse_identifier(ParserErrorType::ExpectedPropertyName)?;
                    ExprType::Get(Box::new(lhs), rhs)
                }
            };

            lhs = to_expr(new_lhs, lhs_span.extend(self.previous.span));
        }

        Ok(lhs)
    }

    /// Parse primary token.
    fn parse_primary(&mut self) -> ParserResult<Expr> {
        self.bump()?;
        let curr_span = self.previous.span;

        let expr = match &self.previous.token {
            Token::Number(n) => from_literal(Literal::Number(*n)),
            Token::True => from_literal(Literal::Boolean(true)),
            Token::False => from_literal(Literal::Boolean(false)),
            Token::String(s) => from_literal(Literal::Str(s.to_owned())),
            Token::Nil => from_literal(Literal::Nil),
            Token::Identifier(name) => {
                ExprType::Variable(Identifier::new(name.to_owned(), curr_span))
            }
            Token::This => ExprType::This,
            Token::Super => {
                self.consume(Token::Dot, ParserErrorType::ExpectedSuperDot)?;
                let method_name = self.parse_identifier(ParserErrorType::ExpectedSuperMethod)?;
                ExprType::Super(method_name)
            }
            Token::LeftParen => {
                let sub_expr = self.parse_expression()?;
                self.consume(
                    Token::RightParen,
                    ParserErrorType::ExpectedAfter(";", Item::Expression),
                )?;
                return Ok(sub_expr);
            }
            t => {
                return Err(ParserError {
                    span: curr_span,
                    error: ParserErrorType::ExpectedExpr(t.clone()),
                })
            }
        };

        Ok(to_expr(expr, curr_span.extend(self.previous.span)))
    }

    fn parse_identifier(&mut self, error: ParserErrorType) -> ParserResult<Identifier> {
        self.bump()?;
        match &self.previous.token {
            Token::Identifier(name) => Ok(Identifier::new(name.to_owned(), self.previous.span)),
            _ => Err(ParserError {
                span: self.previous.span,
                error,
            }),
        }
    }

    fn parse_comma_sep<T, F>(&mut self, parser: F) -> ParserResult<Vec<T>>
    where
        F: Fn(&mut Parser<'s>) -> ParserResult<T>,
    {
        let mut args = vec![];
        if self.check_consume(Token::RightParen) {
            return Ok(args);
        }

        args.push(parser(self)?);

        while !self.check_consume(Token::RightParen) {
            self.consume(Token::Comma, ParserErrorType::ExpectedCommaBetween)?;
            args.push(parser(self)?);
        }

        Ok(args)
    }

    /// Parse function args.
    fn parse_func_args(&mut self) -> ParserResult<Vec<Expr>> {
        self.expect(Token::LeftParen);
        let args = self.parse_comma_sep(Self::parse_expression)?;

        if let Some(extra_args) = args.get(MAX_FUNC_ARGS..) {
            for arg in extra_args.iter() {
                self.emit_error(ParserError {
                    span: arg.span,
                    error: ParserErrorType::TooManyArgs,
                });
            }
        }

        Ok(args)
    }

    fn parse_func_params(&mut self) -> ParserResult<Vec<Identifier>> {
        self.consume(
            Token::LeftParen,
            ParserErrorType::ExpectedAfter("(", Item::FunctionName),
        )?;

        let params = self
            .parse_comma_sep(|this| this.parse_identifier(ParserErrorType::ExpectedIdentifier))?;
        if let Some(extra_params) = params.get(MAX_FUNC_ARGS..) {
            for ident in extra_params.iter() {
                self.emit_error(ParserError {
                    span: ident.span,
                    error: ParserErrorType::TooManyParams,
                });
            }
        }

        Ok(params)
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
