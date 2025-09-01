//! Lua 5.4 Grammar Definition
//! 
//! This module contains the complete Lua 5.4 grammar specification
//! used by our recursive descent parser.

use crate::lexer::TokenType;

/// Lua operators with precedence information
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic (left-associative)
    Add,        // +     (6)
    Sub,        // -     (6)
    Mul,        // *     (7)
    Div,        // /     (7)
    IDiv,       // //    (7)
    Mod,        // %     (7)
    Pow,        // ^     (10, right-associative)
    
    // Bitwise (left-associative)
    BitAnd,     // &     (3)
    BitOr,      // |     (1)
    BitXor,     // ~     (2)
    Shl,        // <<    (4)
    Shr,        // >>    (4)
    
    // String
    Concat,     // ..    (5, right-associative)
    
    // Relational (left-associative)
    Lt,         // <     (8)
    Le,         // <=    (8)
    Gt,         // >     (8)
    Ge,         // >=    (8)
    Eq,         // ==    (8)
    Ne,         // ~=    (8)
    
    // Logical (left-associative)
    And,        // and   (9)
    Or,         // or    (9)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,        // -
    Not,        // not
    Len,        // #
    BitNot,     // ~
}

impl UnaryOp {
    pub fn from_token(token_type: &TokenType) -> Option<Self> {
        match token_type {
            TokenType::Minus => Some(UnaryOp::Neg),
            TokenType::Not => Some(UnaryOp::Not),
            TokenType::Len => Some(UnaryOp::Len),
            TokenType::Tilde => Some(UnaryOp::BitNot),
            _ => None,
        }
    }
    
    /// Get unary operator precedence (unary ops have high precedence, 12)
    pub fn precedence(self) -> u8 {
        12 // Higher than all binary operators
    }
}

impl BinaryOp {
    pub fn from_token(token_type: &TokenType) -> Option<Self> {
        match token_type {
            TokenType::Plus => Some(BinaryOp::Add),
            TokenType::Minus => Some(BinaryOp::Sub),
            TokenType::Star => Some(BinaryOp::Mul),
            TokenType::Slash => Some(BinaryOp::Div),
            TokenType::DoubleSlash => Some(BinaryOp::IDiv),
            TokenType::Percent => Some(BinaryOp::Mod),
            TokenType::Caret => Some(BinaryOp::Pow),
            TokenType::Ampersand => Some(BinaryOp::BitAnd),
            TokenType::Pipe => Some(BinaryOp::BitOr),
            TokenType::Tilde => Some(BinaryOp::BitXor),
            TokenType::LeftShift => Some(BinaryOp::Shl),
            TokenType::RightShift => Some(BinaryOp::Shr),
            TokenType::DoubleDot => Some(BinaryOp::Concat),
            TokenType::Lt => Some(BinaryOp::Lt),
            TokenType::Le => Some(BinaryOp::Le),
            TokenType::Gt => Some(BinaryOp::Gt),
            TokenType::Ge => Some(BinaryOp::Ge),
            TokenType::Eq => Some(BinaryOp::Eq),
            TokenType::Ne => Some(BinaryOp::Ne),
            TokenType::And => Some(BinaryOp::And),
            TokenType::Or => Some(BinaryOp::Or),
            _ => None,
        }
    }

    /// Get operator precedence (higher number = higher precedence)
    /// Lua 5.4 precedence levels from lowest to highest:
    pub fn precedence(self) -> u8 {
        match self {
            BinaryOp::Or => 1,                     // or
            BinaryOp::And => 2,                    // and  
            BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge | BinaryOp::Eq | BinaryOp::Ne => 3, // < <= > >= == ~=
            BinaryOp::BitOr => 4,                  // |
            BinaryOp::BitXor => 5,                 // ~
            BinaryOp::BitAnd => 6,                 // &
            BinaryOp::Shl | BinaryOp::Shr => 7,   // << >>
            BinaryOp::Concat => 8,                 // .. (right-associative)
            BinaryOp::Add | BinaryOp::Sub => 9,   // + -
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::IDiv | BinaryOp::Mod => 10, // * / // %
            BinaryOp::Pow => 11,                   // ^ (right-associative, highest precedence)
        }
    }
    
    /// Whether this operator is right-associative
    pub fn is_right_associative(self) -> bool {
        matches!(self, BinaryOp::Pow | BinaryOp::Concat)
    }
}