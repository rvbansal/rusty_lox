use super::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Dot,
    Comma,
    Semicolon,

    // One or two character tokens.
    Bang,
    BangEq,
    Equals,
    DoubleEq,
    LeftAngle,
    LeftAngleEq,
    RightAngle,
    RightAngleEq,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EndOfFile,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl SpannedToken {
    pub fn new(token: Token, span: Span) -> Self {
        SpannedToken { token, span }
    }

    pub fn split(self) -> (Token, Span) {
        return (self.token, self.span);
    }

    pub fn split_ref(&self) -> (&Token, &Span) {
        let t = (&self.token, &self.span);
        return t;
    }
}
