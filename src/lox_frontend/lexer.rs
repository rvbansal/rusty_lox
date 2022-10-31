use super::cursor::Cursor;
use super::span::Span;
use super::token::{SpannedToken, Token};

pub struct Lexer<'src> {
    source: &'src str,
    cursor: Cursor<'src>,
}

impl<'src> Lexer<'src> {
    /// Creates a lexer from source.
    pub fn new(source: &'src str) -> Self {
        Lexer {
            source,
            cursor: Cursor::new(source),
        }
    }

    /// Returns the next token.
    pub fn next_token(&mut self) -> SpannedToken {
        loop {
            // Get rid of whitespace.
            self.cursor.take_while(|ch| ch.is_ascii_whitespace());

            let start_pos = self.cursor.get_position();
            let token = self.lex_token();
            let end_pos = self.cursor.get_position();

            if let Some(token) = token {
                return SpannedToken {
                    token,
                    span: Span::new(start_pos, end_pos),
                };
            }
        }
    }

    fn lex_token(&mut self) -> Option<Token> {
        let (byte_idx, ch) = match self.cursor.take() {
            Some(t) => t,
            None => return Some(Token::EndOfFile),
        };

        let token = match ch {
            // Single-character tokens.
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '.' => Token::Dot,
            ',' => Token::Comma,
            ';' => Token::Semicolon,

            // Slash can either be comment or division.
            '/' => {
                if self.cursor.take_if('/') {
                    // Comment case.
                    self.consume_line();
                    return None;
                } else {
                    // Division case.
                    Token::Slash
                }
            }

            // Potentially two character tokens.
            '=' => self.look_for_eq_sign(Token::Equals, Token::DoubleEq),
            '<' => self.look_for_eq_sign(Token::LeftAngle, Token::LeftAngleEq),
            '>' => self.look_for_eq_sign(Token::RightAngle, Token::RightAngleEq),
            '!' => self.look_for_eq_sign(Token::Bang, Token::BangEq),

            // String literals.
            '"' => self.lex_string(byte_idx),

            // Numbers.
            _ if is_digit_char(ch) => self.lex_number(byte_idx),

            // Identifiers.
            _ if is_identified_char(ch) => self.lex_identifier_or_kw(byte_idx),

            // Unrecognized token.
            _ => Token::LexerError(format!("Unrecognized token `{}`", ch)),
        };

        Some(token)
    }

    fn consume_line(&mut self) {
        // Consume chars.
        self.cursor.take_while(|ch| ch != '\n');
        // Consume newline char.
        self.cursor.take();
    }

    /// Checks if next char is '='. If so, consume it and return t2.
    /// Otherwise, return t1.
    fn look_for_eq_sign(&mut self, t1: Token, t2: Token) -> Token {
        if self.cursor.take_if('=') {
            t2
        } else {
            t1
        }
    }

    /// Scans string up to next '"' and returns associated string.
    /// start_idx is the starting '"'.
    fn lex_string(&mut self, start_idx: usize) -> Token {
        // Move past starting quote, '"'.
        let start_idx = start_idx + 1;

        self.cursor.take_until(|ch| ch == '"');

        // Move until double quote.
        match self.cursor.peek() {
            Some((end_idx, '"')) => {
                self.cursor.take();
                let string = self.source[start_idx..end_idx].to_owned();
                Token::String(string)
            }
            None => Token::LexerError("No terminal \" in string.".to_owned()),
            _ => unreachable!(),
        }
    }

    /// Scans a number and returns it.
    fn lex_number(&mut self, start_idx: usize) -> Token {
        self.cursor.take_while(is_digit_char);

        // Check for period in float.
        if let Some((_, '.')) = self.cursor.peek() {
            if self
                .cursor
                .peek_next()
                .map_or(false, |t| is_digit_char(t.1))
            {
                self.cursor.take();
                self.cursor.take_while(is_digit_char);
            }
        }

        let end_idx = match self.cursor.peek() {
            None => self.source.len(),
            Some((i, _)) => i,
        };

        let scanned_number = &self.source[start_idx..end_idx];
        match scanned_number.parse() {
            Ok(value) => Token::Number(value),
            Err(_) => Token::LexerError(format!("Unparsable number `{}`", scanned_number)),
        }
    }

    /// Scan up to end of lexemme and return it as identifier. Checks for keywords.
    fn lex_identifier_or_kw(&mut self, start_idx: usize) -> Token {
        self.cursor.take_while(is_identified_char);

        let end_idx = match self.cursor.peek() {
            None => self.source.len(),
            Some((i, _)) => i,
        };

        match &self.source[start_idx..end_idx] {
            "and" => Token::And,
            "class" => Token::Class,
            "else" => Token::Else,
            "false" => Token::False,
            "fun" => Token::Fun,
            "for" => Token::For,
            "if" => Token::If,
            "nil" => Token::Nil,
            "or" => Token::Or,
            "print" => Token::Print,
            "return" => Token::Return,
            "super" => Token::Super,
            "this" => Token::This,
            "true" => Token::True,
            "var" => Token::Var,
            "while" => Token::While,
            other => Token::Identifier(other.to_owned()),
        }
    }

    /// Returns an iterator version of lexer.
    pub fn iter(self) -> LexerIterator<'src> {
        LexerIterator { lexer: self }
    }
}

pub struct LexerIterator<'src> {
    lexer: Lexer<'src>,
}

impl<'src> Iterator for LexerIterator<'src> {
    type Item = SpannedToken;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lexer.next_token();

        if token.token == Token::EndOfFile {
            return None;
        }

        Some(token)
    }
}

fn is_digit_char(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_identified_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}
