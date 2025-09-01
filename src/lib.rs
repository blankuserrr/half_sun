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
use std::marker::PhantomData;
use std::mem::ManuallyDrop; // <-- NEW: Import ManuallyDrop

// Re-export the most important types for easy access
pub use error::{ParseError, Position, Span};
pub use grammar::{BinaryOp, UnaryOp};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// The root of a parsed Lua abstract syntax tree.
///
/// This struct owns the underlying memory for the tree (the Bump allocator
/// and the input string), making it a self-contained unit.
///
/// The `root_block` method provides safe access to the parsed AST.
///
/// # Safety
///
/// This struct uses `unsafe` to store a raw pointer to the root `Block`
/// which points into the owned `arena` and `input` string. This is a common
/// and sound pattern for arena-allocated, self-owning ASTs in Rust,
/// provided the following invariants are upheld:
/// 1. The `arena` and `input` fields must outlive the `root_block_ptr`.
///    This is guaranteed by using `ManuallyDrop` and a custom `Drop`
///    implementation that ensures `arena` and `input` are dropped *after*
///    all references (including the conceptual one from `root_block_ptr`)
///    are considered invalid.
/// 2. The `root_block_ptr` must only be dereferenced when the `Ast` struct
///    is valid (`!std::mem::forget` has been called on it, and it hasn't been moved).
///    This is guaranteed by only exposing `&self` methods for access and
///    by the Rust ownership model for `Ast` itself.
/// 3. The data pointed to by `root_block_ptr` must be properly aligned and initialized.
///    This is guaranteed by `bumpalo` and the parser's allocation.
///
/// This pattern essentially creates a self-referential struct that Rust's safe
/// rules normally prevent, by carefully managing lifetimes and drops with `unsafe`.
pub struct Ast {
    // IMPORTANT: The order of fields matters here for the Drop implementation.
    // `root_block_ptr` logically refers to data within `arena` and `input`.
    // We must drop `arena` and `input` *after* we're done with the `root_block_ptr`.

    // The Bump allocator, which owns the AST nodes.
    // Wrapped in ManuallyDrop to control its drop order.
    arena: ManuallyDrop<Bump>,

    // The input string, which owns the string slices referenced by AST nodes.
    // Wrapped in ManuallyDrop to control its drop order.
    input: ManuallyDrop<String>,

    // A raw pointer to the root Block of the AST.
    // The `'static` lifetime is a placeholder; it indicates that the pointer
    // itself doesn't carry a compile-time checked lifetime. Its validity
    // is tied to the `arena` and `input` fields of *this* struct, managed manually.
    root_block_ptr: *const Block<'static>,

    // PhantomData is crucial here. It tells the compiler that this struct
    // *logically contains* data with the `'static` lifetime (even though
    // it's a raw pointer). This ensures correct variance and auto-trait
    // implementations (like `Send` and `Sync`), even though the underlying
    // references are tied to `self`.
    _phantom: PhantomData<Block<'static>>,
}

impl Ast {
    /// Access the root `Block` of the parsed AST.
    ///
    /// The returned `&Block<'_>` has a lifetime tied to this `Ast` instance.
    pub fn root_block(&self) -> &Block<'_> {
        // SAFETY: See the safety documentation for the `Ast` struct.
        // This dereference is safe because:
        // 1. `self.root_block_ptr` was initialized with a valid pointer
        //    to a `Block` allocated within `self.arena` and referencing `self.input`.
        // 2. `self.arena` and `self.input` are owned by `self` and are guaranteed
        //    to be alive and valid for the duration of `&self`.
        // 3. The lifetime of the returned reference is re-bound to `&self`,
        //    preventing dangling pointers.
        unsafe { &*self.root_block_ptr }
    }
}

/// Implements custom drop logic for `Ast` to correctly deallocate the arena and string.
impl Drop for Ast {
    fn drop(&mut self) {
        // SAFETY: We must manually drop the `arena` and `input` fields in the correct
        // order. The `arena` contains references (string slices) into the `input`
        // string. Therefore, the `arena` must be dropped first to deallocate the AST
        // nodes before the string they point to is dropped. This prevents use-after-free.
        unsafe {
            ManuallyDrop::drop(&mut self.arena);
            ManuallyDrop::drop(&mut self.input);
        }
    }
}

/// Parses Lua source code into an AST.
///
/// On success, it returns a complete `Ast` that owns its data. On failure,
/// it returns the first parsing error encountered.
pub fn parse(code: &str) -> Result<Ast, ParseError> {
    // The arena and input string are created here and will be moved into the Ast struct.
    let arena = ManuallyDrop::new(Bump::new());
    let input_owned = ManuallyDrop::new(code.to_string());

    // Create a parser borrowing from the *owned* arena and input string.
    // The lifetimes here are temporary and only for the duration of this function.
    let mut parser = parser::Parser::new(&input_owned, &arena);

    // Perform the parsing. The `root_block` returned here has a lifetime
    // tied to the `input_owned` and `arena` variables within this function's scope.
    let (root_block, errors) = parser.parse_into_root_block();

    if !errors.is_empty() {
        // For the simple `Result` API, we typically return only the first error.
        return Err(errors.into_iter().next().unwrap());
    }

    // This is the crucial (and safe, given the invariants) `unsafe` block.
    // We cast away the specific lifetimes of `root_block` to `'static`.
    // This is sound because `input_owned` and `arena` are moved *into* the
    // `Ast` struct below, ensuring the memory backing `root_block` lives as
    // long as the `Ast` instance.
    let root_block_static_ptr: *const Block<'static> =
        root_block as *const _ as *const Block<'static>;

    Ok(Ast {
        arena,
        input: input_owned,
        root_block_ptr: root_block_static_ptr,
        _phantom: PhantomData,
    })
}
