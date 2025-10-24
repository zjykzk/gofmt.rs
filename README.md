# gofmt.rs

A Rust implementation of Go language formatting, providing fast and accurate Go source code parsing and formatting capabilities.

## NOT Supported yet

[ ] comments

[ ] simplify

## Features

- **Complete Go Parser**: Full implementation of Go language parsing with proper AST generation
- **Accurate Formatting**: Produces output identical to the official `gofmt` tool
- **High Performance**: Optimized Rust implementation with benchmarking capabilities
- **Comprehensive Testing**: Extensive test suite with golden files covering all Go language constructs
- **Tab-based Formatting**: Proper alignment and indentation using tabs and spaces
- **Comment Preservation**: Maintains and properly formats all types of Go comments

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
gofmt = "0.1.0"
```

## Usage

### Basic Formatting

```rust
use gofmt::formatter::format;

fn main() -> Result<(), String> {
    let go_source = r#"
package main

func main(){
fmt.Println("Hello, World!")
}
"#;

    let formatted = format(go_source)?;
    let result = String::from_utf8(formatted).unwrap();
    println!("{}", result);
    Ok(())
}
```

### Library Structure

The library is organized into several key modules:

- **`formatter`**: Main formatting logic and public API
- **`parser`**: Go language parser implementation
- **`ast`**: Abstract Syntax Tree definitions for Go constructs
- **`scanner`**: Lexical analysis and tokenization
- **`source`**: Source code management and line tracking
- **`tabwriter`**: Tab-based formatting and alignment
- **`tokens`**: Token definitions and precedence rules

## Testing

Run the test suite:

```bash
cargo test
```

Run benchmarks:

```bash
cargo bench
```

### Test Data

The project includes an extensive test suite with over 200 test cases covering:

- **Basic Go constructs**: functions, variables, types, interfaces
- **Complex formatting**: composite literals, function calls, expressions
- **Comment handling**: line comments, block comments, documentation
- **Go build directives**: build tags and compiler directives
- **Advanced features**: generics, type parameters, complex expressions
- **gofumpt compatibility**: Additional formatting rules from gofumpt

Each test case consists of:
- `.input` file: Unformatted Go source code
- `.golden` file: Expected formatted output

## Performance

The formatter is designed for high performance and includes benchmark tests. Run benchmarks to see performance characteristics:

```bash
cargo bench
```

## Project Structure

```
src/
├── lib.rs          # Library entry point
├── formatter.rs    # Main formatting logic
├── parser.rs       # Go language parser
├── ast.rs          # AST node definitions
├── scanner.rs      # Lexical analyzer
├── source.rs       # Source management
├── tabwriter.rs    # Tab formatting
└── tokens.rs       # Token definitions

tests_data/         # Test cases with input/golden files
benches/           # Performance benchmarks
```

## Contributing

Contributions are welcome! This project aims to maintain 100% compatibility with the official Go formatter. When adding features or fixing bugs:

1. Ensure all existing tests pass
2. Add test cases for new functionality
3. Follow Rust best practices and the existing code style
4. Run benchmarks to ensure no performance regressions

## Dependencies

- `strum`: Enum utilities
- `unic-ucd-category`: Unicode character categories
- `anyhow`: Error handling
- `thiserror`: Error derivation
- `unicode-width`: Text width calculations
- `libm`: Math functions
- `criterion`: Benchmarking (dev dependency)

## License

This project follows the same principles as the original Go formatter, focusing on consistent and deterministic code formatting.
