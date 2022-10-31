use super::grammar::{InfixOperator, LogicalOperator};
use super::token::Token;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParserOperator {
    Arithequal(InfixOperator),
    Logical(LogicalOperator),
    Assignment,
    Call,
    Property,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    // Lowest precedence
    Lowest,
    Assignment,
    LogicalOr,
    LogicalAnd,
    Equality,
    Comparison,
    Addition,
    Multiplication,
    Unary,
    Property,
    Call, // Highest precedence
}

#[derive(Debug, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
}

impl ParserOperator {
    pub fn from_token(token: &Token) -> Option<ParserOperator> {
        if let Some(op) = Self::try_arithequal(token) {
            return Some(ParserOperator::Arithequal(op));
        }

        if let Some(op) = Self::try_logical(token) {
            return Some(ParserOperator::Logical(op));
        }

        match token {
            Token::Equals => Some(ParserOperator::Assignment),
            Token::LeftParen => Some(ParserOperator::Call),
            Token::Dot => Some(ParserOperator::Property),
            _ => None,
        }
    }

    fn try_arithequal(token: &Token) -> Option<InfixOperator> {
        let op = match token {
            Token::Plus => InfixOperator::Add,
            Token::Minus => InfixOperator::Subtract,
            Token::Asterisk => InfixOperator::Multiply,
            Token::Slash => InfixOperator::Divide,
            Token::DoubleEq => InfixOperator::EqualTo,
            Token::BangEq => InfixOperator::NotEqualTo,
            Token::RightAngle => InfixOperator::GreaterThan,
            Token::RightAngleEq => InfixOperator::GreaterEq,
            Token::LeftAngle => InfixOperator::LessThan,
            Token::LeftAngleEq => InfixOperator::LessEq,
            _ => return None,
        };
        Some(op)
    }

    fn try_logical(token: &Token) -> Option<LogicalOperator> {
        let op = match token {
            Token::And => LogicalOperator::And,
            Token::Or => LogicalOperator::Or,
            _ => return None,
        };

        Some(op)
    }

    pub fn is_higher_precedence(&self, min_precedence: Precedence) -> bool {
        use std::cmp::Ordering;
        match self.precedence().cmp(&min_precedence) {
            Ordering::Greater => true,
            Ordering::Less => false,
            Ordering::Equal => match self.associativity() {
                Associativity::Left => false,
                Associativity::Right => true,
            },
        }
    }

    pub fn precedence(&self) -> Precedence {
        match self {
            ParserOperator::Arithequal(op) => match op {
                InfixOperator::Add | InfixOperator::Subtract => Precedence::Addition,
                InfixOperator::Multiply | InfixOperator::Divide => Precedence::Multiplication,
                InfixOperator::EqualTo | InfixOperator::NotEqualTo => Precedence::Equality,
                InfixOperator::GreaterEq
                | InfixOperator::LessEq
                | InfixOperator::GreaterThan
                | InfixOperator::LessThan => Precedence::Comparison,
            },
            ParserOperator::Logical(op) => match op {
                LogicalOperator::And => Precedence::LogicalAnd,
                LogicalOperator::Or => Precedence::LogicalOr,
            },
            ParserOperator::Assignment => Precedence::Assignment,
            ParserOperator::Call => Precedence::Call,
            ParserOperator::Property => Precedence::Property,
        }
    }

    pub fn associativity(&self) -> Associativity {
        self.precedence().associativity()
    }
}

impl Precedence {
    fn associativity(&self) -> Associativity {
        match self {
            Precedence::Lowest => unreachable!(),
            Precedence::Assignment => Associativity::Right,
            Precedence::LogicalOr => Associativity::Left,
            Precedence::LogicalAnd => Associativity::Left,
            Precedence::Equality => Associativity::Left,
            Precedence::Comparison => Associativity::Left,
            Precedence::Addition => Associativity::Left,
            Precedence::Multiplication => Associativity::Left,
            Precedence::Unary => unreachable!(),
            Precedence::Property => Associativity::Left,
            Precedence::Call => Associativity::Left,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::more_asserts::*;

    #[test]
    fn test_precedence() {
        assert_lt!(Precedence::Lowest, Precedence::Unary);
        assert_gt!(Precedence::Multiplication, Precedence::Addition);
        assert_gt!(Precedence::Comparison, Precedence::Equality);
    }

    #[test]
    fn test_operators() {
        assert_eq!(
            ParserOperator::from_token(&Token::Plus),
            Some(ParserOperator::Arithequal(InfixOperator::Add))
        );

        assert_eq!(
            ParserOperator::from_token(&Token::Minus),
            Some(ParserOperator::Arithequal(InfixOperator::Subtract))
        );

        assert_eq!(
            ParserOperator::from_token(&Token::And),
            Some(ParserOperator::Logical(LogicalOperator::And))
        );

        assert_eq!(ParserOperator::from_token(&Token::Bang), None);
    }
}
