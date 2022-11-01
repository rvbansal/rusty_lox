use super::span::Span;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PrefixOperator {
    Negate,
    LogicalNot,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InfixOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    EqualTo,
    NotEqualTo,
    GreaterThan,
    GreaterEq,
    LessThan,
    LessEq,
}

#[derive(Debug)]
pub struct Tree {
    pub stmts: Vec<Stmt>,
}

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
    ClassDecl(String, Option<String>, Vec<FuncInfo>),
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
    Variable(String),
    Assignment(String, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    This,
    Super(String),
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

impl LogicalOperator {
    pub fn symbol(&self) -> &str {
        match self {
            LogicalOperator::And => "and",
            LogicalOperator::Or => "or",
        }
    }
}

impl PrefixOperator {
    pub fn symbol(&self) -> &str {
        match self {
            PrefixOperator::LogicalNot => "!",
            PrefixOperator::Negate => "-",
        }
    }
}

impl InfixOperator {
    pub fn symbol(&self) -> &str {
        match self {
            InfixOperator::Add => "+",
            InfixOperator::Subtract => "-",
            InfixOperator::Multiply => "*",
            InfixOperator::Divide => "/",
            InfixOperator::EqualTo => "==",
            InfixOperator::NotEqualTo => "!=",
            InfixOperator::GreaterThan => ">",
            InfixOperator::GreaterEq => ">=",
            InfixOperator::LessThan => "<",
            InfixOperator::LessEq => "<=",
        }
    }
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
            ExprType::Variable(var) => var.clone(),
            ExprType::Assignment(var, expr) => format!("(set {} {})", var, expr.ast_string()),
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
            ExprType::This => String::from("this"),
            ExprType::Super(method) => format!("(super {})", method),
        }
    }
}
