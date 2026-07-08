.PHONY: compile assembly native clean run help clean-vampire clean-vamplean build-deps


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
		native-image -jar target/scala-2.13/valeadate.jar --gc=G1 -O3  valeadate && \
		echo "Native executable created at: ./valeadate"; \
	else \
		echo "Error: native-image not found. Install GraalVM native-image."; \
		exit 1; \
	fi

# Build static native image (musl)
native-static: assembly
	@echo "Building statically linked native image (musl) with GraalVM..."
	@command -v native-image >/dev/null 2>&1 || { echo "Error: native-image not found. Install GraalVM native-image."; exit 1; }
	@command -v musl-gcc >/dev/null 2>&1 && \
		native-image -jar target/scala-2.13/valeadate.jar --static -O3 --libc=musl -H:Name=valeadate-musl -H:-ReduceImplicitExceptionStackTraceInformation -H:NativeLinkerOption=-L/usr/local/musl/lib && \
		echo "Static native executable created at: ./valeadate-musl" || \
		{ echo "Error: musl-gcc not found or native-image failed. Install musl toolchain (e.g. musl-tools) or use GraalVM musl builder image."; exit 1; }

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

# Initialize Vampire submodule

build-deps: vampire/build/vampire vamplean/.lake/build/lib/lean/VampLean.olean 

clean-vampire:
	@echo "Cleaning Vampire build artifacts..."
	rm -rf vampire/build
	@echo "Vampire clean complete"

clean-vamplean:
	@echo "Cleaning VampLean build artifacts..."
	rm -rf vamplean/.lake/build
	@echo "VampLean clean complete"

vampire/.git:
	git submodule update --init vampire; \
	
# Build Vampire
vampire/build/vampire: vampire/.git
	@echo "Compiling Vampire..."
	cd vampire && mkdir -p build && cd build && cmake .. && make -j 8
	@echo "Vampire compilation complete"

# Initialize VampLean submodule
vamplean/VampLean.lean:
	git submodule update --init --recursive vamplean; \
		
# Build VampLean
vamplean/.lake/build/lib/lean/VampLean.olean : vamplean/VampLean.lean
	cd vamplean && lake build
