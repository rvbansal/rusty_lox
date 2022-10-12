use super::cursor::Cursor;
use super::span::Span;
use super::token::{SpannedToken, Token};

fn is_digit_char(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_identified_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

pub struct Lexer<'src> {
    source: &'src str,
    cursor: Cursor<'src>,
    seen_eof: bool,
}

pub type LexerResult<T> = Result<T, String>;

impl<'src> Lexer<'src> {
    /// Creates a lexer from source code.
    pub fn new(source: &'src str) -> Self {
        Lexer {
            source,
            cursor: Cursor::new(source),
            seen_eof: false,
        }
    }

    /// Returns the next token or None when the string is finished.
    fn scan_token(&mut self) -> Option<LexerResult<SpannedToken>> {
        // Get rid of whitespace.
        self.cursor.take_while(|ch| ch.is_ascii_whitespace());

        // Store starting position.
        let start_pos = self.cursor.get_position();

        // Handle EOF case.
        let (byte_idx, ch) = match self.cursor.take() {
            Some(t) => t,
            None => {
                if self.seen_eof {
                    return None;
                }
                self.seen_eof = true;
                let span = Span::new(start_pos, start_pos);
                let token = Token::EndOfFile;
                return Some(Ok(SpannedToken::new(token, span)));
            }
        };

        let token_result = match ch {
            // Single-character tokens.
            '(' => Ok(Token::LeftParen),
            ')' => Ok(Token::RightParen),
            '{' => Ok(Token::LeftBrace),
            '}' => Ok(Token::RightBrace),
            '+' => Ok(Token::Plus),
            '-' => Ok(Token::Minus),
            '*' => Ok(Token::Asterisk),
            '.' => Ok(Token::Dot),
            ',' => Ok(Token::Comma),
            ';' => Ok(Token::Semicolon),

            // Slash can either be comment or division.
            '/' => {
                if self.cursor.take_if('/') {
                    // Comment case.
                    self.consume_line();
                    return self.scan_token();
                } else {
                    // Division case.
                    Ok(Token::Slash)
                }
            }

            // Potentially two character tokens.
            '=' => Ok(self.look_for_eq_sign(Token::Equals, Token::DoubleEq)),
            '<' => Ok(self.look_for_eq_sign(Token::LeftAngle, Token::LeftAngleEq)),
            '>' => Ok(self.look_for_eq_sign(Token::RightAngle, Token::RightAngleEq)),
            '!' => Ok(self.look_for_eq_sign(Token::Bang, Token::BangEq)),

            // String literals.
            '"' => self.scan_string(byte_idx),

            // Numbers.
            _ if is_digit_char(ch) => self.scan_number(byte_idx),

            // Identifiers.
            _ if is_identified_char(ch) => self.scan_identifier_or_kw(byte_idx),

            // Unrecognized token.
            _ => Err(format!("Unrecognized token `{}`", ch)),
        };

        // If we get valid token, return it. Else bubble up error.
        match token_result {
            Ok(token) => {
                let end_pos = self.cursor.get_position();
                let span = Span::new(start_pos, end_pos);
                let token = SpannedToken::new(token, span);
                Some(Ok(token))
            }
            Err(e) => Some(Err(format!("{}: {}", start_pos, e))),
        }
    }

    fn consume_line(&mut self) {
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
    fn scan_string(&mut self, start_idx: usize) -> LexerResult<Token> {
        // Move past starting quote, '"'.
        let start_idx = start_idx + 1;

        self.cursor.take_until(|ch| ch == '"');

        let end_idx = match self.cursor.peek() {
            None => return Err("Unterminated string.".to_owned()),
            Some((i, _)) => i,
        };

        // Consume ending quote, '"'.
        self.cursor.take();

        let scanned_string = self.source[start_idx..end_idx].to_owned();
        Ok(Token::String(scanned_string))
    }

    /// Scans a number and returns it.
    fn scan_number(&mut self, start_idx: usize) -> LexerResult<Token> {
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
            Ok(value) => Ok(Token::Number(value)),
            Err(_) => Err(format!("Unparsable number `{}`", scanned_number)),
        }
    }

    /// Scan up to end of lexemme and return it as identifier. Checks for keywords.
    fn scan_identifier_or_kw(&mut self, start_idx: usize) -> LexerResult<Token> {
        self.cursor.take_while(is_identified_char);

        let end_idx = match self.cursor.peek() {
            None => self.source.len(),
            Some((i, _)) => i,
        };

        let slice = &self.source[start_idx..end_idx];
        let token = match slice {
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
            _ => Token::Identifier(slice.to_owned()),
        };

        Ok(token)
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
    type Item = LexerResult<SpannedToken>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.scan_token()
    }
}
