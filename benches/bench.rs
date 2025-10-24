use criterion::{Criterion, criterion_group, criterion_main};
use gofmt::formatter::format;
use std::fmt::Write;

fn array1(n: usize) -> String {
    let mut buf = String::new();
    buf.push_str("let _ = [\n");

    let mut i = 0;
    while i < n {
        if i % 10 == 0 {
            writeln!(buf, "    // {}", i).unwrap();
        }
        buf.push_str("    ");

        for _ in 0..8 {
            if i >= n {
                break;
            }
            write!(buf, "0x{:02x}, ", i as u8).unwrap();
            i += 1;
        }
        buf.push_str("\n");
    }

    buf.push_str("];\n");
    buf
}

fn bench_format(c: &mut Criterion) {
    let source = array1(10000);
    c.bench_function("format", |b| {
        b.iter(|| {
            let _ = format(&source);
        });
    });
}

criterion_group!(benches, bench_format);
criterion_main!(benches);
