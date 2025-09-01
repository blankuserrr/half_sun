use crate::error::Span;

/// A trait representing any node in the AST.
pub trait Node {
    /// Returns the `Span` of the node in the original source code.
    fn span(&self) -> Span;
}
