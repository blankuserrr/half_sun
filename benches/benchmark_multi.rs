use criterion::{Criterion, criterion_group, criterion_main};
use std::fs;
use std::hint::black_box;

fn criterion_benchmark(c: &mut Criterion) {
    let code = fs::read_to_string("examples/input.lua").expect("Unable to read input.lua");
    let mut group = c.benchmark_group("parsers");
    group.bench_function("half_sun", |b| {
        b.iter(|| black_box(half_sun::parse(&code).unwrap()))
    });
    group.bench_function("full_moon", |b| {
        b.iter(|| black_box(full_moon::parse(&code).unwrap()))
    });
    group.bench_function("lua-parser", |b| {
        b.iter(|| black_box(lua_parser::parse_str(&code).unwrap()))
    });
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
