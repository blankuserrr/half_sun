// Core modules defining the structure of the language
pub mod ast;
pub mod error;
pub mod grammar;
pub mod node;
pub mod visitors;

// Low-level tokenizer, for users who need it
pub mod lexer;

// Internal implementation details
mod parser;

use crate::ast::Block; // Use Block directly as the root type
use bumpalo::Bump;
use mimalloc::MiMalloc;
use ouroboros::self_referencing;

// Re-export the most important types for easy access
pub use error::{ParseError, Position, Span};
pub use grammar::{BinaryOp, UnaryOp};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// The root of a parsed Lua abstract syntax tree, created using `ouroboros`
/// to safely manage the self-referential relationship between the input string,
/// the arena allocator, and the parsed AST nodes.
#[self_referencing]
#[derive(Debug)]
pub struct Ast {
    /// The input string, which owns the string slices referenced by AST nodes.
    input: String,
    /// The Bump allocator, which owns the AST nodes.
    arena: Bump,

    /// A reference to the root `Block` of the AST.
    /// The lifetime `'this` is managed by `ouroboros` and ensures this
    /// reference cannot outlive `input` or `arena`.
    #[borrows(arena, input)]
    #[covariant]
    root_block: &'this Block<'this>,
}

impl Ast {
    /// Access the root `Block` of the parsed AST.
    ///
    /// The returned `&Block<'_>` has a lifetime tied to this `Ast` instance.
    pub fn root_block(&self) -> &Block<'_> {
        self.borrow_root_block()
    }
}

/// Parses Lua source code into an AST.
///
/// On success, it returns a complete `Ast` that owns its data. On failure,
/// it returns the first parsing error encountered.
pub fn parse(code: &str) -> Result<Ast, ParseError> {
    Ast::try_new(code.to_string(), Bump::new(), |arena, input| {
        // The `ouroboros` builder provides references to the owned fields
        // (`arena` and `input`) with the appropriate lifetime. We can use
        // these to construct the parser and generate the AST.
        let mut parser = parser::Parser::new(input, arena);
        let (root_block, errors) = parser.parse_into_root_block();

        if !errors.is_empty() {
            Err(errors.into_iter().next().unwrap())
        } else {
            // The `root_block` has its lifetime tied to `arena` and `input`,
            // which is exactly what `ouroboros` needs to store it safely.
            Ok(root_block)
        }
    })
}
