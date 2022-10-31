use super::constants::{MAX_FUNC_ARGS, SUPER_STR, THIS_STR};
use super::grammar::PrefixOperator;
use super::grammar::{Expr, ExprType, FuncInfo, Literal, Stmt, StmtType, VariableInfo};
use super::lexer::Lexer;
use super::parser_utils::{ParserOperator, Precedence};
use super::span::Span;
use super::token::{SpannedToken, Token};

pub struct Parser<'s> {
    source: &'s str,
    lexer: Lexer<'s>,
    current: SpannedToken,
    previous: SpannedToken,
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedToken(Token, Span, Token),
    ExpectedExpr(Span, Token),
    ExpectedIdentifier(Span),
    TooManyArgs(Span),
    ExpectedLValue(Span),
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
        std::mem::drop(());
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
                self.previous.span,
                self.previous.token.clone(),
            ))
        }
    }

    /// Parses program from the top of treating it as a set of statements.
    pub fn parse(mut self) -> Vec<ParserResult<Stmt>> {
        let mut stmts = vec![];

        while !self.check(Token::EndOfFile) {
            let stmt = self.parse_spanned_declaration();
            stmts.push(stmt);
        }

        stmts
    }

    /// Handle variable declations separately from non-declaring statements since
    /// they may not be allowed everywhere non-declaring statements are allowed.
    fn parse_spanned_declaration(&mut self) -> ParserResult<Stmt> {
        let span_start = self.current.span;
        let decl = self.parse_nospan_declaration()?;
        let span_end = self.previous.span;

        Ok(Stmt {
            stmt: decl,
            span: span_start.extend(span_end),
        })
    }

    fn parse_nospan_declaration(&mut self) -> ParserResult<StmtType> {
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

                Ok(StmtType::VariableDecl(name, expr))
            }
            Token::Fun => {
                let func_info = self.parse_func_info(false)?;

                Ok(StmtType::FuncDecl(func_info))
            }
            Token::Class => self.parse_class_decl(),
            _ => self.parse_nospan_statement(),
        }
    }

    /// Parse func info into func info struct.
    fn parse_func_info(&mut self, is_method: bool) -> ParserResult<FuncInfo> {
        if !is_method {
            self.consume(Token::Fun)?;
        }
        let name = self.parse_identifier()?;
        let params = self.parse_func_params()?;

        let span_start = self.current.span;
        let body = self.parse_block()?;
        let span_end = self.previous.span;

        let body = to_stmt(body, span_start.extend(span_end));

        let func_info = FuncInfo::new(name, params, body);

        Ok(func_info)
    }

    /// Parse class declaration.
    fn parse_class_decl(&mut self) -> ParserResult<StmtType> {
        let _curr_span = self.current.span;

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

        Ok(StmtType::ClassDecl(name, superclass, methods_info))
    }

    fn parse_spanned_statement(&mut self) -> ParserResult<Stmt> {
        let span_start = self.current.span;
        let stmt = self.parse_nospan_statement()?;
        let span_end = self.previous.span;

        Ok(Stmt {
            stmt,
            span: span_start.extend(span_end),
        })
    }

    fn parse_nospan_statement(&mut self) -> ParserResult<StmtType> {
        match self.current.token {
            Token::Print => {
                self.bump();
                let expr = self.parse_expression()?;
                self.consume(Token::Semicolon)?;

                Ok(StmtType::Print(expr))
            }
            Token::If => self.parse_if_else(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Return => self.parse_return(),
            Token::LeftBrace => self.parse_block(),
            _ => {
                let expr = self.parse_expression()?;
                self.consume(Token::Semicolon)?;

                Ok(StmtType::Expression(expr))
            }
        }
    }

    fn parse_if_else(&mut self) -> ParserResult<StmtType> {
        let _curr_span = self.current.span;

        self.consume(Token::If)?;
        self.consume(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume(Token::RightParen)?;
        let if_body = Box::new(self.parse_spanned_statement()?);
        let else_body = if self.check_consume(Token::Else) {
            Some(Box::new(self.parse_spanned_statement()?))
        } else {
            None
        };

        Ok(StmtType::IfElse(condition, if_body, else_body))
    }

    fn parse_while(&mut self) -> ParserResult<StmtType> {
        let _curr_span = self.current.span;

        self.consume(Token::While)?;
        self.consume(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume(Token::RightParen)?;
        let body = self.parse_spanned_statement()?;

        Ok(StmtType::While(condition, Box::new(body)))
    }

    /// Parse for loop.
    fn parse_for(&mut self) -> ParserResult<StmtType> {
        let curr_span = self.current.span;

        self.consume(Token::For)?;
        self.consume(Token::LeftParen)?;

        // Get initializer. It can be empty.
        let init_stmt = if self.check_consume(Token::Semicolon) {
            None
        } else if self.check(Token::Var) {
            Some(self.parse_spanned_declaration()?)
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
        let mut body = self.parse_spanned_statement()?;

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

        Ok(body.stmt)
    }

    /// Parse return statement.
    fn parse_return(&mut self) -> ParserResult<StmtType> {
        self.consume(Token::Return)?;
        let expr = if !self.check(Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume(Token::Semicolon)?;

        Ok(StmtType::Return(expr))
    }

    fn parse_block(&mut self) -> ParserResult<StmtType> {
        let _curr_span = self.current.span;

        let mut stmts = vec![];
        self.consume(Token::LeftBrace)?;
        while !self.check_consume(Token::RightBrace) {
            stmts.push(self.parse_spanned_declaration()?);
        }

        Ok(StmtType::Block(stmts))
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
                self.bump();
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

            if op != ParserOperator::Call {
                self.bump();
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
                        ExprType::Variable(var_info) => {
                            ExprType::Assignment(VariableInfo::new(var_info.name), rhs_box)
                        }
                        ExprType::Get(expr, property) => ExprType::Set(expr, property, rhs_box),
                        _ => return Err(ParserError::ExpectedLValue(lhs.span)),
                    }
                }
                ParserOperator::Call => {
                    let arguments = self.parse_func_args()?;
                    ExprType::Call(Box::new(lhs), arguments)
                }
                ParserOperator::Property => {
                    let rhs = self.parse_identifier()?;
                    ExprType::Get(Box::new(lhs), rhs)
                }
            };

            lhs = to_expr(new_lhs, lhs_span.extend(self.previous.span));
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
            t => return Err(ParserError::ExpectedExpr(curr_span, t.clone())),
        };

        Ok(to_expr(expr, curr_span.extend(self.previous.span)))
    }

    fn parse_identifier(&mut self) -> ParserResult<String> {
        self.bump();
        match &self.previous.token {
            Token::Identifier(name) => Ok(name.to_owned()),
            _ => Err(ParserError::ExpectedIdentifier(self.previous.span)),
        }
    }

    fn parse_comma_sep<T, F>(&mut self, parser: F, max_elements: usize) -> ParserResult<Vec<T>>
    where
        F: Fn(&mut Parser<'s>) -> ParserResult<T>,
    {
        self.consume(Token::LeftParen)?;

        let mut args = vec![];
        if self.check_consume(Token::RightParen) {
            return Ok(args);
        }

        args.push(parser(self)?);
        while !self.check_consume(Token::RightParen) {
            self.consume(Token::Comma)?;
            args.push(parser(self)?);
        }

        if args.len() >= max_elements {
            return Err(ParserError::TooManyArgs(self.current.span));
        }

        Ok(args)
    }

    /// Parse function args.
    fn parse_func_args(&mut self) -> ParserResult<Vec<Expr>> {
        self.parse_comma_sep(Self::parse_expression, MAX_FUNC_ARGS)
    }

    fn parse_func_params(&mut self) -> ParserResult<Vec<String>> {
        self.parse_comma_sep(Self::parse_identifier, MAX_FUNC_ARGS)
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
