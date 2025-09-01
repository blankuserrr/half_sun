use anyhow::Result;
use half_sun::{
    self,
    ast::{Expression, Statement},
    visitors::Visitor,
};
use std::fs;

fn main() -> Result<()> {
    println!("Half Sun - Blazingly Fast Lua Parser");

    let args: Vec<String> = std::env::args().collect();
    let filename = args.get(1).map(|s| s.as_str()).unwrap_or("input.lua");

    let code = fs::read_to_string(filename)?;
    println!("Parsing {}...", filename);

    match half_sun::parse(&code) {
        Ok(ast) => {
            println!("✓ {} parsed successfully", filename);

            // Demonstrate actual AST usage
            let root_block = ast.root_block();
            let statement_count = root_block.statements.len();
            let has_return = root_block.return_stmt.is_some();

            println!("  - Parsed {} statements", statement_count);
            if has_return {
                println!("  - Has return statement");
            }

            // Demonstrate visitor usage
            println!("\n=== Running Visitor to Count Nodes ===");
            let mut counter = NodeCounter::default();
            root_block.accept(&mut counter);
            println!("  - Statements: {}", counter.statements);
            println!("  - Expressions: {}", counter.expressions);
        }
        Err(error) => {
            println!("✗ Found an error while parsing:");
            println!("  - {}", error);
        }
    }

    Ok(())
}

#[derive(Default)]
struct NodeCounter {
    statements: usize,
    expressions: usize,
}

impl<'arena> Visitor<'arena> for NodeCounter {
    fn visit_statement(&mut self, _stmt: &Statement<'arena>) {
        self.statements += 1;
        half_sun::visitors::walk_statement(self, _stmt);
    }

    fn visit_expression(&mut self, _expr: &Expression<'arena>) {
        self.expressions += 1;
        half_sun::visitors::walk_expression(self, _expr);
    }
}
