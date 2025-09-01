//! Error types for the Half Sun Lua parser
//!
//! This module defines comprehensive error types for both lexical analysis
//! and parsing phases, designed for excellent error reporting and debugging.

use std::fmt;
use thiserror::Error;

/// Position information for error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self {
            line,
            column,
            offset,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}", self.line)
    }
}

/// Span representing a range in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn single(pos: Position) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}", self.start.line)
    }
}

/// Lexical analysis errors
#[derive(Debug, Error, Clone)]
pub enum LexError {
    #[error("Invalid character '{character}' at {position}")]
    InvalidCharacter { character: char, position: Position },

    #[error("Unterminated string literal at {position}")]
    UnterminatedString { position: Position },

    #[error("Unterminated multi-line comment at {position}")]
    UnterminatedComment { position: Position },

    #[error("Malformed number at {position}")]
    InvalidNumber { position: Position },

    #[error("Invalid escape sequence '\\{sequence}' at {position}")]
    InvalidEscape {
        sequence: String,
        position: Position,
    },
}

/// Parse errors
#[derive(Debug, Error, Clone)]
pub enum ParseError {
    #[error(transparent)]
    LexerError(#[from] LexError),
    #[error("Unexpected {found} at {span}, expected {expected}")]
    UnexpectedToken {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Invalid syntax at {span}: {message}")]
    InvalidSyntax { message: String, span: Span },
}

impl ParseError {
    pub fn span(&self) -> Option<Span> {
        match self {
            ParseError::LexerError(_) => None,
            ParseError::UnexpectedToken { span, .. } => Some(*span),
            ParseError::InvalidSyntax { span, .. } => Some(*span),
        }
    }
}

/// Unified error type for the parser
pub type ParseResult<T> = std::result::Result<T, ParseError>;
pub type LexResult<T> = std::result::Result<T, LexError>;
