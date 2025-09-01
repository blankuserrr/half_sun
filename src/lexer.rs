//! Zero-copy lexer for Lua source code
//! 
//! This lexer is designed for maximum performance with zero-copy token extraction
//! and efficient preservation of comments and whitespace for lossless parsing.

use crate::error::{LexError, LexResult, Position, Span};
use std::str::Chars;
use std::iter::Peekable;

/// Token types representing all Lua language elements
#[derive(Debug, PartialEq)]
pub enum TokenType<'a> {
    // Literals
    Number(&'a str),
    String(&'a str),
    Identifier(&'a str),
    
    // Keywords
    And, Break, Do, Else, ElseIf, End, False, For, Function,
    Goto, If, In, Local, Nil, Not, Or, Repeat, Return,
    Then, True, Until, While,
    
    // Operators
    Plus, Minus, Star, Slash, DoubleSlash, Percent, Caret,
    Ampersand, Pipe, Tilde, LeftShift, RightShift,
    DoubleDot, Lt, Le, Gt, Ge, Eq, Ne, Len,
    
    // Punctuation
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    Semicolon, Comma, Dot, Colon, DoubleColon, Assign,
    
    // Special
    Ellipsis,    // ...
    
    // Trivia (preserved for lossless parsing)
    Whitespace(&'a str),
    LineComment(&'a str),
    BlockComment(&'a str),
    
    // Meta
    Eof,
}

/// A token with its type, span, and source text
#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType<'a>, span: Span) -> Self {
        Self { token_type, span }
    }
}

/// High-performance zero-copy lexer
pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    position: Position,
    current_offset: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given input
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().peekable(),
            position: Position::new(1, 1, 0),
            current_offset: 0,
        }
    }
    
    /// Get the next token from the input stream, including trivia
    pub fn next_token(&mut self) -> LexResult<Token<'a>> {
        if self.is_at_end() {
            return Ok(self.make_token(TokenType::Eof, self.position, self.position));
        }
    
        let start_pos = self.position;
        let start_offset = self.current_offset;
    
        if let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.advance();
                while let Some(ch) = self.peek_char() {
                    if ch.is_whitespace() {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let text = &self.input[start_offset..self.current_offset];
                let span = Span::new(start_pos, self.position);
                return Ok(Token::new(TokenType::Whitespace(text), span));
            } else if ch == '-' {
                if self.input[self.current_offset..].starts_with("--") {
                    // It's a comment.
                    if self.input[self.current_offset..].starts_with("--[") {
                        // Block comment
                        return self.read_block_comment(start_pos, start_offset);
                    } else {
                        // Line comment
                        return self.read_line_comment(start_pos, start_offset);
                    }
                }
            }
        }
    
        // If no trivia, read a significant token
        self.read_significant_token()
    }
    
    /// Read a significant (non-trivia) token
    fn read_significant_token(&mut self) -> LexResult<Token<'a>> {
        if self.is_at_end() {
            return Ok(self.make_token(TokenType::Eof, self.position, self.position));
        }
        
        let start_pos = self.position;
        let start_offset = self.current_offset;
        
        match self.peek_char() {
            Some(ch) => {
                let token_type = match ch {
                    // Single character tokens
                    '+' => { self.advance(); TokenType::Plus }
                    '-' => { self.advance(); TokenType::Minus }
                    '*' => { self.advance(); TokenType::Star }
                    '%' => { self.advance(); TokenType::Percent }
                    '^' => { self.advance(); TokenType::Caret }
                    '&' => { self.advance(); TokenType::Ampersand }
                    '|' => { self.advance(); TokenType::Pipe }
                    '~' => { 
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            TokenType::Ne
                        } else {
                            TokenType::Tilde
                        }
                    }
                    '(' => { self.advance(); TokenType::LeftParen }
                    ')' => { self.advance(); TokenType::RightParen }
                    '{' => { self.advance(); TokenType::LeftBrace }
                    '}' => { self.advance(); TokenType::RightBrace }
                    '[' => { 
                        self.advance();
                        // Check for long string/comment
                        if self.peek_char() == Some('[') || self.peek_char() == Some('=') {
                            return self.read_long_bracket_content(start_pos, start_offset);
                        }
                        TokenType::LeftBracket
                    }
                    ']' => { self.advance(); TokenType::RightBracket }
                    ';' => { self.advance(); TokenType::Semicolon }
                    ',' => { self.advance(); TokenType::Comma }
                    ':' => {
                        self.advance();
                        if self.peek_char() == Some(':') {
                            self.advance();
                            TokenType::DoubleColon
                        } else {
                            TokenType::Colon
                        }
                    }
                    '=' => {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            TokenType::Eq
                        } else {
                            TokenType::Assign
                        }
                    }
                    '<' => {
                        self.advance();
                        match self.peek_char() {
                            Some('=') => { self.advance(); TokenType::Le }
                            Some('<') => { self.advance(); TokenType::LeftShift }
                            _ => TokenType::Lt,
                        }
                    }
                    '>' => {
                        self.advance();
                        match self.peek_char() {
                            Some('=') => { self.advance(); TokenType::Ge }
                            Some('>') => { self.advance(); TokenType::RightShift }
                            _ => TokenType::Gt,
                        }
                    }
                    '/' => {
                        self.advance();
                        if self.peek_char() == Some('/') {
                            self.advance();
                            TokenType::DoubleSlash
                        } else {
                            TokenType::Slash
                        }
                    }
                    '.' => {
                        self.advance();
                        match self.peek_char() {
                            Some('.') => {
                                self.advance();
                                if self.peek_char() == Some('.') {
                                    self.advance();
                                    TokenType::Ellipsis
                                } else {
                                    TokenType::DoubleDot
                                }
                            }
                            Some(c) if c.is_ascii_digit() => {
                                return self.read_number(start_pos, start_offset);
                            }
                            _ => TokenType::Dot,
                        }
                    }
                    '"' | '\'' => return self.read_string(start_pos, start_offset),
                    '#' => { self.advance(); TokenType::Len }
                    c if c.is_ascii_digit() => return self.read_number(start_pos, start_offset),
                    c if c.is_alphabetic() || c == '_' => return self.read_identifier_or_keyword(start_pos, start_offset),
                    c => return Err(LexError::InvalidCharacter { 
                        character: c, 
                        position: start_pos 
                    }),
                };
                
                let end_pos = self.position;
                Ok(Token::new(token_type, Span::new(start_pos, end_pos)))
            }
            None => Ok(self.make_token(TokenType::Eof, start_pos, start_pos)),
        }
    }
    
    /// Peek at the current character without consuming it
    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }
    
    /// Advance to the next character and update position
    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.chars.next() {
            if ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }
            self.position.offset += ch.len_utf8();
            self.current_offset += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }
    
    /// Check if we're at the end of input
    fn is_at_end(&mut self) -> bool {
        self.peek_char().is_none()
    }
    
    /// Create a token with the given type and span
    pub fn make_token(&self, token_type: TokenType<'a>, start: Position, end: Position) -> Token<'a> {
        let span = Span::new(start, end);
        Token::new(token_type, span)
    }

    /// Get the lexer's current position
    pub fn current_position(&self) -> Position {
        self.position
    }
    
    /// Read a string literal (quoted) with escape sequence validation
    fn read_string(&mut self, start_pos: Position, start_offset: usize) -> LexResult<Token<'a>> {
        let quote_char = self.advance().ok_or(LexError::UnterminatedString { position: start_pos })?; // consume opening quote
        
        while let Some(ch) = self.peek_char() {
            if ch == quote_char {
                self.advance(); // consume closing quote
                break;
            } else if ch == '\\' {
                self.advance(); // consume backslash
                if let Some(escaped_ch) = self.peek_char() {
                    match escaped_ch {
                        'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '"' | '\'' | '\n' => {
                            self.advance(); // valid escape sequence
                        }
                        'z' => {
                            // \z skips subsequent whitespace
                            self.advance();
                            while let Some(ch) = self.peek_char() {
                                if ch.is_whitespace() {
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                        }
                        'x' => {
                            // \xXX hexadecimal escape
                            self.advance(); // consume 'x'
                            for _ in 0..2 {
                                if let Some(hex_ch) = self.peek_char() {
                                    if hex_ch.is_ascii_hexdigit() {
                                        self.advance();
                                    } else {
                                        return Err(LexError::InvalidEscape { 
                                            sequence: format!("x{}", hex_ch), 
                                            position: self.position 
                                        });
                                    }
                                } else {
                                    return Err(LexError::InvalidEscape { 
                                        sequence: "x".to_string(), 
                                        position: self.position 
                                    });
                                }
                            }
                        }
                        'u' => {
                            // \u{XXX} Unicode escape (Lua 5.4)
                            self.advance(); // consume 'u'
                            if self.peek_char() == Some('{') {
                                self.advance(); // consume '{'
                                let mut hex_digits = 0;
                                
                                // Read hex digits
                                while let Some(hex_ch) = self.peek_char() {
                                    if hex_ch.is_ascii_hexdigit() {
                                        self.advance();
                                        hex_digits += 1;
                                        if hex_digits > 6 {
                                            return Err(LexError::InvalidEscape { 
                                                sequence: "u{...}".to_string(), 
                                                position: self.position 
                                            });
                                        }
                                    } else if hex_ch == '}' {
                                        break;
                                    } else {
                                        return Err(LexError::InvalidEscape { 
                                            sequence: format!("u{{{}", hex_ch), 
                                            position: self.position 
                                        });
                                    }
                                }
                                
                                // Must have at least one hex digit and close with '}'
                                if hex_digits == 0 {
                                    return Err(LexError::InvalidEscape { 
                                        sequence: "u{}".to_string(), 
                                        position: self.position 
                                    });
                                }
                                
                                if self.peek_char() == Some('}') {
                                    self.advance(); // consume '}'
                                } else {
                                    return Err(LexError::InvalidEscape { 
                                        sequence: "u{...".to_string(), 
                                        position: self.position 
                                    });
                                }
                            } else {
                                return Err(LexError::InvalidEscape { 
                                    sequence: "u".to_string(), 
                                    position: self.position 
                                });
                            }
                        }
                        '0'..='9' => {
                            // \ddd decimal escape (up to 3 digits)
                            for _ in 0..3 {
                                if let Some(digit_ch) = self.peek_char() {
                                    if digit_ch.is_ascii_digit() {
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }
                        }
                        _ => {
                            return Err(LexError::InvalidEscape { 
                                sequence: escaped_ch.to_string(), 
                                position: self.position 
                            });
                        }
                    }
                } else {
                    return Err(LexError::UnterminatedString { position: start_pos });
                }
            } else if ch == '\n' {
                return Err(LexError::UnterminatedString { position: start_pos });
            } else {
                self.advance();
            }
        }
        
        // Check if we reached EOF without finding closing quote
        if self.is_at_end() {
            return Err(LexError::UnterminatedString { position: start_pos });
        }
        
        let text = &self.input[start_offset..self.current_offset];
        let span = Span::new(start_pos, self.position);
        Ok(Token::new(TokenType::String(text), span))
    }
    
    /// Read a number literal with strict validation (like Lua 5.4)
    fn read_number(&mut self, start_pos: Position, start_offset: usize) -> LexResult<Token<'a>> {
        let is_hex = self.input[start_offset..].starts_with("0x") || self.input[start_offset..].starts_with("0X");

        if is_hex {
            self.advance(); // 0
            self.advance(); // x or X

            let hex_start_offset = self.current_offset;
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_hexdigit() {
                    self.advance();
                } else {
                    break;
                }
            }
            if self.current_offset == hex_start_offset {
                return Err(LexError::InvalidNumber { position: start_pos });
            }
        } else {
            // Parse integer part
            self.consume_digits();
            
            // Parse optional decimal part
            if self.peek_char() == Some('.') {
                let remainder = &self.input[self.current_offset..];
                if remainder.len() > 1 && remainder.as_bytes()[1].is_ascii_digit() {
                    self.advance(); // consume '.'
                    self.consume_digits();
                }
            }
            
            // Parse optional exponent part
            if let Some(ch) = self.peek_char() {
                if ch == 'e' || ch == 'E' {
                    let save_offset = self.current_offset;
                    let save_position = self.position;
                    
                    self.advance(); // consume 'e' or 'E'
                    
                    // Optional sign
                    if let Some(sign) = self.peek_char() {
                        if sign == '+' || sign == '-' {
                            self.advance();
                        }
                    }
                    
                    // Exponent digits are required
                    if !self.consume_digits() {
                        // No digits after exponent, backtrack
                        self.reset_to_offset(save_offset, save_position);
                    }
                }
            }
        }
        
        // After parsing, check for invalid trailing characters.
        if let Some(ch) = self.peek_char() {
            if ch.is_alphabetic() { // e.g. 123a
                return Err(LexError::InvalidNumber { position: start_pos });
            }
        }
        
        let text = &self.input[start_offset..self.current_offset];
        let span = Span::new(start_pos, self.position);
        Ok(Token::new(TokenType::Number(text), span))
    }
    
    /// Consume consecutive digits, returning true if any were consumed
    fn consume_digits(&mut self) -> bool {
        let mut consumed = false;
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                self.advance();
                consumed = true;
            } else {
                break;
            }
        }
        consumed
    }
    
    /// Read an identifier or keyword
    fn read_identifier_or_keyword(&mut self, start_pos: Position, start_offset: usize) -> LexResult<Token<'a>> {
        while let Some(ch) = self.peek_char() {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        
        let text = &self.input[start_offset..self.current_offset];
        let span = Span::new(start_pos, self.position);
        
        let token_type = match text {
            "and" => TokenType::And,
            "break" => TokenType::Break,
            "do" => TokenType::Do,
            "else" => TokenType::Else,
            "elseif" => TokenType::ElseIf,
            "end" => TokenType::End,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "function" => TokenType::Function,
            "goto" => TokenType::Goto,
            "if" => TokenType::If,
            "in" => TokenType::In,
            "local" => TokenType::Local,
            "nil" => TokenType::Nil,
            "not" => TokenType::Not,
            "or" => TokenType::Or,
            "repeat" => TokenType::Repeat,
            "return" => TokenType::Return,
            "then" => TokenType::Then,
            "true" => TokenType::True,
            "until" => TokenType::Until,
            "while" => TokenType::While,
            _ => TokenType::Identifier(text),
        };
        
        Ok(Token::new(token_type, span))
    }
    
    /// Read a line comment (starting with --)
    fn read_line_comment(&mut self, start_pos: Position, start_offset: usize) -> LexResult<Token<'a>> {
        self.advance(); // consume '-'
        self.advance(); // consume '-'

        // Consume until end of line
        while let Some(ch) = self.peek_char() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
        
        let text = &self.input[start_offset..self.current_offset];
        let span = Span::new(start_pos, self.position);
        Ok(Token::new(TokenType::LineComment(text), span))
    }
    
    /// Read a block comment (starting with --[[ or --[=[)
    fn read_block_comment(&mut self, start_pos: Position, start_offset: usize) -> LexResult<Token<'a>> {
        // Consume '--'
        self.advance(); // first '-'
        self.advance(); // second '-'
        
        // Now we should be at '['
        self.advance(); // consume '['
        
        // Parse the bracket level
        let level = self.parse_long_bracket_level()?;
        
        if let Some(bracket_level) = level {
            // Consume the final '[' of the opening bracket
            if self.peek_char() == Some('[') {
                self.advance(); // consume final '['
                
                // Skip the optional newline immediately after opening bracket
                if self.peek_char() == Some('\n') {
                    self.advance();
                }
                
                // Find the matching closing bracket
                loop {
                    if let Some(']') = self.peek_char() {
                        if self.try_match_closing_bracket(bracket_level) {
                            break; // Found matching closing bracket
                        }
                    }

                    if self.advance().is_none() {
                        return Err(LexError::UnterminatedComment { position: start_pos });
                    }
                }
            } else {
                return Err(LexError::UnterminatedComment { position: start_pos });
            }
        } else {
            // Treat as line comment starting with --[
            while let Some(ch) = self.peek_char() {
                if ch == '\n' {
                    break;
                }
                self.advance();
            }
        }
        
        let text = &self.input[start_offset..self.current_offset];
        let span = Span::new(start_pos, self.position);
        Ok(Token::new(TokenType::BlockComment(text), span))
    }
    
    /// Read long bracket content (strings or comments)
    fn read_long_bracket_content(&mut self, start_pos: Position, start_offset: usize) -> LexResult<Token<'a>> {
        // Parse the opening bracket level (count '=' signs)
        let level = self.parse_long_bracket_level()?;
        
        if level.is_none() {
            // Not a long bracket, treat as regular bracket
            let span = Span::new(start_pos, self.position);
            return Ok(Token::new(TokenType::LeftBracket, span));
        }
        
        let bracket_level = level.unwrap();
        
        // Consume the final '[' of the opening bracket
        if self.peek_char() != Some('[') {
            // Not a long bracket, treat as regular bracket
            let span = Span::new(start_pos, self.position);
            return Ok(Token::new(TokenType::LeftBracket, span));
        }
        self.advance(); // consume final '['
        
        // Skip the optional newline immediately after opening bracket
        if self.peek_char() == Some('\n') {
            self.advance();
        }
        
        // Find the matching closing bracket
        loop {
            if let Some(']') = self.peek_char() {
                // Check if this is the matching closing bracket
                if self.try_match_closing_bracket(bracket_level) {
                    break; // Found matching closing bracket
                }
            }
            if self.advance().is_none() {
                return Err(LexError::UnterminatedComment { position: start_pos });
            }
        }
        
        let text = &self.input[start_offset..self.current_offset];
        let span = Span::new(start_pos, self.position);
        Ok(Token::new(TokenType::String(text), span))
    }
    
    /// Parse the level of a long bracket (count '=' characters)
    /// Returns None if this is not a valid long bracket start
    fn parse_long_bracket_level(&mut self) -> LexResult<Option<usize>> {
        let mut level = 0;
        
        // We've already consumed the first '[', now count '=' characters
        while self.peek_char() == Some('=') {
            self.advance();
            level += 1;
        }
        
        // Must be followed by '[' to be a valid long bracket
        if self.peek_char() == Some('[') {
            Ok(Some(level))
        } else {
            Ok(None)
        }
    }
    
    /// Try to match a closing bracket of the given level
    /// Returns true if matched, false otherwise
    /// If false, resets position to before the ']'
    fn try_match_closing_bracket(&mut self, expected_level: usize) -> bool {
        let save_offset = self.current_offset;
        let save_position = self.position;
        
        // Consume the ']'
        let consumed = self.advance();
        if consumed != Some(']') {
            return false;
        }
        
        // Count '=' characters
        let mut level = 0;
        while self.peek_char() == Some('=') {
            self.advance();
            level += 1;
        }
        
        // Must end with ']' and have the right level
        if self.peek_char() == Some(']') && level == expected_level {
            self.advance(); // consume final ']'
            true
        } else {
            // Reset position AND chars iterator
            self.reset_to_offset(save_offset, save_position);
            false
        }
    }
    
    /// Reset lexer state to a specific offset and position
    fn reset_to_offset(&mut self, offset: usize, position: Position) {
        self.current_offset = offset;
        self.position = position;
        // Reconstruct the chars iterator from the correct position
        // Make sure we don't go beyond the input bounds
        if offset <= self.input.len() {
            self.chars = self.input[offset..].chars().peekable();
        } else {
            // If offset is beyond bounds, create an empty iterator
            self.chars = "".chars().peekable();
        }
    }
}

/// Iterator interface for the lexer
impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<Token<'a>>;
    
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) if matches!(token.token_type, TokenType::Eof) => None,
            result => Some(result),
        }
    }
}
