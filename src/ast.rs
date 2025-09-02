#![allow(dead_code)]
//! Abstract Syntax Tree definitions for Lua
//!
//! This module defines the AST nodes using arena allocation for maximum performance
//! and zero-copy string references for minimal memory overhead.

use crate::error::Span;
use crate::grammar::{BinaryOp, UnaryOp};
use crate::node::Node;
use crate::visitors::Visitor;
use bumpalo::Bump;

/// Common slice aliases to simplify list types across the AST
pub type List<'arena, T> = &'arena [T];
pub type RefList<'arena, T> = &'arena [&'arena T];
pub type StrList<'arena> = &'arena [&'arena str];

// Macros to reduce boilerplate for Node implementations
macro_rules! impl_node_struct {
    ($name:ident $(<$($gen:tt),*>)?) => {
        impl$(<$($gen),*>)? Node for $name$(<$($gen),*>)? {
            fn span(&self) -> Span { self.span }
        }
    };
}

macro_rules! impl_node_enum_with_span {
    ($name:ident $(<$($gen:tt),*>)?, $($variant:ident),+ $(,)?) => {
        impl$(<$($gen),*>)? Node for $name$(<$($gen),*>)? {
            fn span(&self) -> Span {
                match self {
                    $( Self::$variant { span, .. } => *span, )+
                }
            }
        }
    };
}

/// A block is a sequence of statements with an optional return statement
#[derive(Debug, Clone)]
pub struct Block<'arena> {
    pub statements: List<'arena, Statement<'arena>>,
    pub return_stmt: Option<&'arena ReturnStatement<'arena>>,
    pub span: Span,
}

impl_node_struct!(Block<'arena>);

impl<'arena> Block<'arena> {
    pub fn accept<V: Visitor<'arena>>(&self, visitor: &mut V) {
        visitor.visit_block(self);
    }
}

/// Lua statements
#[derive(Debug, Clone)]
pub enum Statement<'arena> {
    /// Empty statement (just a semicolon)
    Empty { span: Span },

    /// Variable assignment: varlist = explist
    Assignment {
        variables: RefList<'arena, Variable<'arena>>,
        expressions: List<'arena, Expression<'arena>>,
        span: Span,
    },

    /// An expression used as a statement (typically a function call)
    Expression {
        expression: &'arena Expression<'arena>,
        span: Span,
    },

    /// Label statement: ::label::
    Label { name: &'arena str, span: Span },

    /// Break statement
    Break { span: Span },

    /// Goto statement: goto label
    Goto { label: &'arena str, span: Span },

    /// Do block end
    Do {
        block: &'arena Block<'arena>,
        span: Span,
    },

    /// While loop: while exp do block end
    While {
        condition: &'arena Expression<'arena>,
        body: &'arena Block<'arena>,
        span: Span,
    },

    /// Repeat loop: repeat block until exp
    Repeat {
        body: &'arena Block<'arena>,
        condition: &'arena Expression<'arena>,
        span: Span,
    },

    /// If statement: if exp then block {elseif exp then block} [else block] end
    If {
        condition: &'arena Expression<'arena>,
        then_block: &'arena Block<'arena>,
        elseif_clauses: List<'arena, ElseIfClause<'arena>>,
        else_block: Option<&'arena Block<'arena>>,
        span: Span,
    },

    /// Numeric for loop: for name = exp, exp [, exp] do block end
    NumericFor {
        variable: &'arena str,
        start: &'arena Expression<'arena>,
        end: &'arena Expression<'arena>,
        step: Option<&'arena Expression<'arena>>,
        body: &'arena Block<'arena>,
        span: Span,
    },

    /// Generic for loop: for namelist in explist do block end
    GenericFor {
        variables: StrList<'arena>,
        expressions: List<'arena, Expression<'arena>>,
        body: &'arena Block<'arena>,
        span: Span,
    },

    /// Function definition: function funcname funcbody
    FunctionDef {
        name: &'arena FunctionName<'arena>,
        body: &'arena FunctionBody<'arena>,
        span: Span,
    },

    /// Local function: local function name funcbody
    LocalFunction {
        name: &'arena str,
        body: &'arena FunctionBody<'arena>,
        span: Span,
    },

    /// Local variables: local attnamelist [= explist]
    LocalVariables {
        names: List<'arena, AttributedName<'arena>>,
        expressions: Option<List<'arena, Expression<'arena>>>,
        span: Span,
    },
}

impl<'arena> Statement<'arena> {
    pub fn accept<V: Visitor<'arena>>(&self, visitor: &mut V) {
        visitor.visit_statement(self);
    }
}

impl_node_enum_with_span!(
    Statement<'arena>,
    Empty,
    Assignment,
    Expression,
    Label,
    Break,
    Goto,
    Do,
    While,
    Repeat,
    If,
    NumericFor,
    GenericFor,
    FunctionDef,
    LocalFunction,
    LocalVariables,
);

/// Return statement
#[derive(Debug, Clone)]
pub struct ReturnStatement<'arena> {
    pub expressions: Option<List<'arena, Expression<'arena>>>,
    pub span: Span,
}

impl_node_struct!(ReturnStatement<'arena>);

/// ElseIf clause for if statements
#[derive(Debug, Clone)]
pub struct ElseIfClause<'arena> {
    pub condition: &'arena Expression<'arena>,
    pub block: &'arena Block<'arena>,
    pub span: Span,
}

impl_node_struct!(ElseIfClause<'arena>);

/// Function name (can be dotted with optional method)
#[derive(Debug, Clone)]
pub struct FunctionName<'arena> {
    pub base: &'arena str,
    pub fields: StrList<'arena>,
    pub method: Option<&'arena str>,
    pub span: Span,
}

impl_node_struct!(FunctionName<'arena>);

/// Function body
#[derive(Debug, Clone)]
pub struct FunctionBody<'arena> {
    pub parameters: StrList<'arena>,
    pub is_vararg: bool,
    pub body: &'arena Block<'arena>,
    pub span: Span,
}

impl_node_struct!(FunctionBody<'arena>);

/// Variable name with optional attribute
#[derive(Debug, Clone)]
pub struct AttributedName<'arena> {
    pub name: &'arena str,
    pub attribute: Option<&'arena str>,
    pub span: Span,
}

impl_node_struct!(AttributedName<'arena>);

/// Variables (lvalues)
#[derive(Debug, Clone)]
pub enum Variable<'arena> {
    /// Simple identifier
    Identifier { name: &'arena str, span: Span },

    /// Table field access: exp[exp]
    Index {
        table: &'arena Expression<'arena>,
        index: &'arena Expression<'arena>,
        span: Span,
    },

    /// Table field access: exp.name
    Field {
        table: &'arena Expression<'arena>,
        field: &'arena str,
        span: Span,
    },
}

impl_node_enum_with_span!(Variable<'arena>, Identifier, Index, Field);

/// Expressions
#[derive(Debug, Clone)]
pub enum Expression<'arena> {
    /// Literals
    Nil {
        span: Span,
    },
    Boolean {
        value: bool,
        span: Span,
    },
    Number {
        value: &'arena str,
        span: Span,
    },
    String {
        value: &'arena str,
        span: Span,
    },

    /// Variable reference
    Variable {
        var: &'arena Variable<'arena>,
        span: Span,
    },

    /// Function call
    FunctionCall {
        function: &'arena Expression<'arena>,
        method: Option<&'arena str>,
        arguments: List<'arena, Expression<'arena>>,
        span: Span,
    },

    /// Binary operation
    BinaryOp {
        left: &'arena Expression<'arena>,
        operator: BinaryOp,
        right: &'arena Expression<'arena>,
        span: Span,
    },

    /// Unary operation
    UnaryOp {
        operator: UnaryOp,
        operand: &'arena Expression<'arena>,
        span: Span,
    },

    /// Table constructor
    Table {
        fields: List<'arena, TableField<'arena>>,
        span: Span,
    },

    /// Function definition: function funcbody
    Function {
        body: &'arena FunctionBody<'arena>,
        span: Span,
    },

    /// Varargs: ...
    Varargs {
        span: Span,
    },

    /// Parenthesized expression
    Parenthesized {
        expression: &'arena Expression<'arena>,
        span: Span,
    },
}

impl<'arena> Expression<'arena> {
    pub fn accept<V: Visitor<'arena>>(&self, visitor: &mut V) {
        visitor.visit_expression(self);
    }
}

impl_node_enum_with_span!(
    Expression<'arena>,
    Nil,
    Boolean,
    Number,
    String,
    Variable,
    FunctionCall,
    BinaryOp,
    UnaryOp,
    Table,
    Function,
    Varargs,
    Parenthesized,
);

/// Table field
#[derive(Debug, Clone)]
pub enum TableField<'arena> {
    /// [exp] = exp
    Index {
        key: &'arena Expression<'arena>,
        value: &'arena Expression<'arena>,
        span: Span,
    },

    /// name = exp
    Named {
        key: &'arena str,
        value: &'arena Expression<'arena>,
        span: Span,
    },

    /// exp (list-style)
    List {
        value: &'arena Expression<'arena>,
        span: Span,
    },
}

impl_node_enum_with_span!(TableField<'arena>, Index, Named, List);

/// AST builder for convenient construction with arena allocation
pub struct AstBuilder<'arena> {
    arena: &'arena Bump,
}

impl<'arena> AstBuilder<'arena> {
    pub fn new(arena: &'arena Bump) -> Self {
        Self { arena }
    }

    /// Allocate a slice in the arena
    pub fn alloc_slice<T>(&self, items: &[T]) -> List<'arena, T>
    where
        T: Clone,
    {
        self.arena.alloc_slice_clone(items)
    }

    /// Allocate a single item in the arena
    pub fn alloc<T>(&self, item: T) -> &'arena T {
        self.arena.alloc(item)
    }

    /// Allocate a string slice in the arena
    pub fn alloc_str(&self, s: &str) -> &'arena str {
        self.arena.alloc_str(s)
    }
}
