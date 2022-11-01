use crate::lox_frontend::grammar as frontend_grammar;
use crate::lox_frontend::span::Span;

pub type Literal = frontend_grammar::Literal;
pub type InfixOperator = frontend_grammar::InfixOperator;
pub type PrefixOperator = frontend_grammar::PrefixOperator;
pub type LogicalOperator = frontend_grammar::LogicalOperator;

#[derive(Debug, Clone)]
pub struct Tree {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub stmt: StmtType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtType {
    Expression(Expr),
    Print(Expr),
    VariableDecl(String, Expr),
    Block(Vec<Stmt>),
    IfElse(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    FuncDecl(FuncInfo),
    Return(Expr),
    ClassDecl(String, Option<VariableInfo>, Vec<FuncInfo>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub expr: ExprType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprType {
    Literal(Literal),
    Infix(InfixOperator, Box<Expr>, Box<Expr>),
    Prefix(PrefixOperator, Box<Expr>),
    Logical(LogicalOperator, Box<Expr>, Box<Expr>),
    Variable(VariableInfo),
    Assignment(VariableInfo, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    This(VariableInfo),
    Super(VariableInfo, String),
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub env_hops: VHops,
}

#[derive(Debug, Clone)]
pub struct FuncInfo {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum VHops {
    Global,
    Local(usize),
}
