.PHONY: help compile assembly native clean run help

# Default target
help:
	@echo "VaLeaDate - Scala TPTP Parser"
	@echo ""
	@echo "Available targets:"
	@echo "  make compile      - Compile the Scala project with SBT"
	@echo "  make assembly     - Build fat JAR for GraalVM native-image"
	@echo "  make native       - Build native executable with GraalVM"
	@echo "  make run FILE=... - Run the application (FILE is required)"
	@echo "  make run-native   - Run the native executable"
	@echo "  make clean        - Remove build artifacts"
	@echo "  make help         - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make compile"
	@echo "  make assembly"
	@echo "  make native"
	@echo "  make run FILE=/path/to/file.tptp"
	@echo "  make clean"

# Compile the project
compile:
	@echo "Compiling Scala project..."
	sbt compile

# Build assembly JAR
assembly: compile
	@echo "Building assembly JAR..."
	sbt assembly
	@echo "JAR created at: target/scala-2.13/valeadate.jar"

# Build native image with GraalVM
native: assembly
	@echo "Building native image with GraalVM..."
	@if command -v native-image >/dev/null 2>&1; then \
		native-image -jar target/scala-2.13/valeadate.jar -march=native valeadate && \
		echo "Native executable created at: ./valeadate"; \
	else \
		echo "Error: native-image not found. Install GraalVM native-image."; \
		exit 1; \
	fi

# Run the application
run:
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required"; \
		echo "Usage: make run FILE=/path/to/file.tptp"; \
		exit 1; \
	fi
	@echo "Running application with file: $(FILE)"
	sbt "run $(FILE)"

# Run with verbose output
run-verbose:
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required"; \
		echo "Usage: make run-verbose FILE=/path/to/file.tptp"; \
		exit 1; \
	fi
	@echo "Running application with verbose output: $(FILE)"
	sbt "run $(FILE) -v"

# Run native executable
run-native:
	@if [ ! -f ./valeadate ]; then \
		echo "Error: Native executable not found. Run 'make native' first."; \
		exit 1; \
	fi
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required"; \
		echo "Usage: make run-native FILE=/path/to/file.tptp"; \
		exit 1; \
	fi
	@echo "Running native executable with file: $(FILE)"
	./valeadate $(FILE)

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	sbt clean
	rm -f valeadate
	@echo "Clean complete"

# Watch mode - recompile on changes
watch:
	@echo "Watching for changes..."
	sbt "~compile"
