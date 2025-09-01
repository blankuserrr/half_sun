use std::fs;
use std::time::Instant;

const ITERATIONS: u32 = 100;

fn main() {
    let code = fs::read_to_string("example_inputs/input.lua").expect("Unable to read input.lua");
    println!("File size: {} bytes", code.len());
    println!("Running {} iterations for each parser.", ITERATIONS);
    println!();

    println!("Benching half_sun...");
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        // Prevent the optimizer from removing the call
        std::hint::black_box(half_sun::parse(&code).unwrap());
    }
    let duration = start.elapsed();
    println!(
        "half_sun took: {:?} (avg: {:?})",
        duration,
        duration / ITERATIONS
    );
    println!();

    println!("Benching full_moon...");
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        std::hint::black_box(full_moon::parse(&code).unwrap());
    }
    let duration = start.elapsed();
    println!(
        "full_moon took: {:?} (avg: {:?})",
        duration,
        duration / ITERATIONS
    );
    println!();

    println!("Benching lua-parser...");
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        std::hint::black_box(lua_parser::parse_str(&code).unwrap());
    }
    let duration = start.elapsed();
    println!(
        "lua-parser took: {:?} (avg: {:?})",
        duration,
        duration / ITERATIONS
    );
}
