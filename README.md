# VaLeaDate


## Prerequisites

- Scala 2.13.12 or later
- SBT 1.9.7 or later
- Java 11 or later

## Build

```bash
sbt compile
```

## Build with Makefile

```bash
# Show available targets
make help

# Compile the project
make compile

# Build assembly JAR
make assembly

# Build native image
make native

# Clean artifacts
make clean
```

## Run

```bash
sbt "run <input-file> [options]"
```

Or with Makefile:

```bash
make run FILE=/path/to/file.tptp
make run-verbose FILE=/path/to/file.tptp
```

### Options

- `<input>` (required) - Input TPTP file
- `-o, --output FILE` - Output file (optional)
- `-v, --verbose` - Enable verbose output
- `-h, --help` - Show help message

### Examples

Parse a TPTP file:
```bash
sbt "run example.tptp"
# or
make run FILE=example.tptp
```

Parse with verbose output:
```bash
sbt "run example.tptp -v"
# or
make run-verbose FILE=example.tptp
```

Parse and save the proof DAG as Graphviz DOT:
```bash
sbt "run example.tptp -o output.dot"
```

The default output is a DOT representation of the inference DAG inferred from the proof annotations. You can render it with Graphviz, for example:
```bash
dot -Tpng output.dot -o proof.png
```


## Development

Run in development mode with watch:
```bash
sbt "~compile"
```

## GraalVM Native Image Compilation

Build a native executable using GraalVM:

### Prerequisites
- Install GraalVM with native-image component

### Build Native Image

1. Create a fat JAR:
   ```bash
   sbt assembly
   ```

2. Compile to native executable:
   ```bash
   native-image -jar target/scala-2.13/valeadate.jar valeadate
   ```

3. Run the native executable:
   ```bash
   ./ /path/to/file.tptp
   ```

The resulting `valeadate` executable is a standalone binary with no JVM dependency, resulting in faster startup times and lower memory usage.

## Dependencies

- `scala-tptp-parser_2.13` (1.7.4) - TPTP parser library
- `scopt` (4.1.0) - Command-line argument parser
