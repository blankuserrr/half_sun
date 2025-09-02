use anyhow::Result;
use half_sun::{self};
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

            println!("\n=== Full AST ===");
            // Show the full AST using Debug formatting
            println!("{:#?}", dbg!(ast));
        }
        Err(error) => {
            println!("✗ Found an error while parsing:");
            println!("  - {}", error);
        }
    }

    Ok(())
}
