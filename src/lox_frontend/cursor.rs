use super::span::CodePosition;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, Clone)]
pub struct Cursor<'src> {
    source: &'src str,
    char_iterator: Peekable<CharIndices<'src>>,
    position: CodePosition,
}

impl<'src> Cursor<'src> {
    /// Creates a character stream for the source string.
    pub fn new(source: &'src str) -> Self {
        Cursor {
            source,
            char_iterator: source.char_indices().peekable(),
            position: CodePosition::new(0, 1, 1),
        }
    }

    /// Position of the cursor.
    pub fn get_position(&self) -> CodePosition {
        self.position
    }

    /// Peeks the next character without consuming it.
    pub fn peek(&mut self) -> Option<(usize, char)> {
        self.char_iterator.peek().copied()
    }

    /// Peeks the next to next character without consuming it.
    pub fn peek_next(&mut self) -> Option<(usize, char)> {
        let mut temp_cursor = self.clone();
        temp_cursor.take();
        temp_cursor.peek()
    }

    /// Consumes the next character.
    pub fn take(&mut self) -> Option<(usize, char)> {
        let (byte_idx, ch) = match self.char_iterator.next() {
            None => return None,
            Some(t) => t,
        };

        self.position.byte_pos = self.peek().map(|(idx, _)| idx).unwrap_or(self.source.len());
        if ch == '\n' {
            self.position.line_no += 1;
            self.position.column_no = 1;
        } else {
            self.position.column_no += 1;
        }

        Some((byte_idx, ch))
    }

    /// Consumes the next character if it equals target char.
    pub fn take_if(&mut self, target: char) -> bool {
        match self.peek() {
            None => false,
            Some((_, ch)) if ch != target => false,
            _ => {
                self.take();
                true
            }
        }
    }

    /// Consumes next characters as long as they meet condition.
    /// At the end, the next character fails condition.
    pub fn take_while<F>(&mut self, condition: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            match self.peek() {
                Some((_, ch)) if condition(ch) => {
                    self.take();
                }
                _ => break,
            }
        }
    }

    /// Consumes next characters as long as they do not meet condition.
    /// At the end, the next character meets condition.
    pub fn take_until<F>(&mut self, condition: F)
    where
        F: Fn(char) -> bool,
    {
        self.take_while(|ch| !condition(ch));
    }
}
