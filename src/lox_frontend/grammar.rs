use super::operator::{InfixOperator, LogicalOperator, PrefixOperator};
use super::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct Stmt {
    pub stmt: StmtType,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StmtType {
    Expression(Expr),
    Print(Expr),
    VariableDecl(String, Expr),
    Block(Vec<Stmt>),
    IfElse(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    FuncDecl(FuncInfo),
    Return(Option<Expr>),
    ClassDecl(String, Option<VariableInfo>, Vec<FuncInfo>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub expr: ExprType,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub env_hops: Option<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncInfo {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    Boolean(bool),
    Str(String),
    Nil,
}

impl Stmt {
    pub fn new(stmt: StmtType, span: Span) -> Self {
        Stmt { stmt, span }
    }
}

impl Expr {
    pub fn new(expr: ExprType, span: Span) -> Self {
        Expr { expr, span }
    }
}

impl VariableInfo {
    pub fn new(name: String) -> Self {
        VariableInfo {
            name,
            env_hops: None,
        }
    }
}

impl FuncInfo {
    pub fn new(name: String, params: Vec<String>, body: Stmt) -> Self {
        FuncInfo {
            name,
            params,
            body: Box::new(body),
        }
    }
}

impl Expr {
    pub fn ast_string(&self) -> String {
        match &self.expr {
            ExprType::Literal(l) => match l {
                Literal::Number(n) => n.to_string(),
                Literal::Boolean(b) => b.to_string(),
                Literal::Str(s) => format!("\"{}\"", s),
                Literal::Nil => "nil".to_owned(),
            },
            ExprType::Infix(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.ast_string(),
                rhs.ast_string()
            ),
            ExprType::Prefix(op, expr) => format!("({} {})", op.symbol(), expr.ast_string()),
            ExprType::Logical(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.ast_string(),
                rhs.ast_string()
            ),
            ExprType::Variable(var) => var.name.clone(),
            ExprType::Assignment(var, expr) => format!("(set {} {})", var.name, expr.ast_string()),
            ExprType::Call(callee, args) => {
                let exprs: Vec<_> = args.iter().map(|a| a.ast_string()).collect();
                format!("(call {} {})", callee.ast_string(), exprs.join(" "))
            }
            ExprType::Get(expr, property) => format!("(get {} {})", expr.ast_string(), property),
            ExprType::Set(expr_lhs, property, expr_rhs) => format!(
                "(set {} {} {})",
                expr_lhs.ast_string(),
                property,
                expr_rhs.ast_string()
            ),
            ExprType::This(_) => String::from("this"),
            ExprType::Super(_, method) => format!("(super {})", method),
        }
    }
}
