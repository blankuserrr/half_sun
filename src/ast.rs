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

/// A block is a sequence of statements with an optional return statement
#[derive(Debug, Clone)]
pub struct Block<'arena> {
    pub statements: &'arena [Statement<'arena>],
    pub return_stmt: Option<&'arena ReturnStatement<'arena>>,
    pub span: Span,
}

impl Node for Block<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

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
        variables: &'arena [&'arena Variable<'arena>],
        expressions: &'arena [Expression<'arena>],
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
        elseif_clauses: &'arena [ElseIfClause<'arena>],
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
        variables: &'arena [&'arena str],
        expressions: &'arena [Expression<'arena>],
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
        names: &'arena [AttributedName<'arena>],
        expressions: Option<&'arena [Expression<'arena>]>,
        span: Span,
    },
}

impl<'arena> Statement<'arena> {
    pub fn accept<V: Visitor<'arena>>(&self, visitor: &mut V) {
        visitor.visit_statement(self);
    }
}

impl Node for Statement<'_> {
    fn span(&self) -> Span {
        match self {
            Statement::Empty { span } => *span,
            Statement::Assignment { span, .. } => *span,
            Statement::Expression { span, .. } => *span,
            Statement::Label { span, .. } => *span,
            Statement::Break { span } => *span,
            Statement::Goto { span, .. } => *span,
            Statement::Do { span, .. } => *span,
            Statement::While { span, .. } => *span,
            Statement::Repeat { span, .. } => *span,
            Statement::If { span, .. } => *span,
            Statement::NumericFor { span, .. } => *span,
            Statement::GenericFor { span, .. } => *span,
            Statement::FunctionDef { span, .. } => *span,
            Statement::LocalFunction { span, .. } => *span,
            Statement::LocalVariables { span, .. } => *span,
        }
    }
}

/// Return statement
#[derive(Debug, Clone)]
pub struct ReturnStatement<'arena> {
    pub expressions: Option<&'arena [Expression<'arena>]>,
    pub span: Span,
}

impl Node for ReturnStatement<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

/// ElseIf clause for if statements
#[derive(Debug, Clone)]
pub struct ElseIfClause<'arena> {
    pub condition: &'arena Expression<'arena>,
    pub block: &'arena Block<'arena>,
    pub span: Span,
}

impl Node for ElseIfClause<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

/// Function name (can be dotted with optional method)
#[derive(Debug, Clone)]
pub struct FunctionName<'arena> {
    pub base: &'arena str,
    pub fields: &'arena [&'arena str],
    pub method: Option<&'arena str>,
    pub span: Span,
}

impl Node for FunctionName<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

/// Function body
#[derive(Debug, Clone)]
pub struct FunctionBody<'arena> {
    pub parameters: &'arena [&'arena str],
    pub is_vararg: bool,
    pub body: &'arena Block<'arena>,
    pub span: Span,
}

impl Node for FunctionBody<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

/// Variable name with optional attribute
#[derive(Debug, Clone)]
pub struct AttributedName<'arena> {
    pub name: &'arena str,
    pub attribute: Option<&'arena str>,
    pub span: Span,
}

impl Node for AttributedName<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

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

impl Node for Variable<'_> {
    fn span(&self) -> Span {
        match self {
            Variable::Identifier { span, .. } => *span,
            Variable::Index { span, .. } => *span,
            Variable::Field { span, .. } => *span,
        }
    }
}

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
        arguments: &'arena [Expression<'arena>],
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
        fields: &'arena [TableField<'arena>],
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

impl Node for Expression<'_> {
    fn span(&self) -> Span {
        match self {
            Expression::Nil { span } => *span,
            Expression::Boolean { span, .. } => *span,
            Expression::Number { span, .. } => *span,
            Expression::String { span, .. } => *span,
            Expression::Variable { span, .. } => *span,
            Expression::FunctionCall { span, .. } => *span,
            Expression::BinaryOp { span, .. } => *span,
            Expression::UnaryOp { span, .. } => *span,
            Expression::Table { span, .. } => *span,
            Expression::Function { span, .. } => *span,
            Expression::Varargs { span } => *span,
            Expression::Parenthesized { span, .. } => *span,
        }
    }
}

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

impl Node for TableField<'_> {
    fn span(&self) -> Span {
        match self {
            TableField::Index { span, .. } => *span,
            TableField::Named { span, .. } => *span,
            TableField::List { span, .. } => *span,
        }
    }
}

/// AST builder for convenient construction with arena allocation
pub struct AstBuilder<'arena> {
    arena: &'arena Bump,
}

impl<'arena> AstBuilder<'arena> {
    pub fn new(arena: &'arena Bump) -> Self {
        Self { arena }
    }

    /// Allocate a slice in the arena
    pub fn alloc_slice<T>(&self, items: &[T]) -> &'arena [T]
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
