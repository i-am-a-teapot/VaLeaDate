# VaLeaDate

## Build

## Quickinstall

A script which *should* handle the whole setup procedure: 

```./quickinstall```



If something fails there, here is a brief guide on how to install VaLeaDate:

<details>
  <summary>Detailed installing instructions</summary>

  ### Install prerequisites

VaLeaDate is written in Scala, uses GraalVM to build a native binary and requires Vampire (a custom branch), Lean and VampLean (a Lean Library). 

GraalVM, SBT and Lean are installed (locally) with the following commands:

```
source ./sourceRequirements.sh
```

you can also install them systemwide, but make sure to set the paths properly (take a look into the ./sourceRequirements.sh script)

### Compile Vampire & VampLean

At this stage, a working lean install is required (provided by the previous script) + standard build-tools

```
make build-deps
```

### Compile VaLeaDate (native)

```
make native
```
</details>

## Running VaLeaDate

VaLeaDate requires several binarys to function properly and can get their paths either from the environment variables or command-line arguments.

These are the relevant environment variables (with their default values)
```
export VAMPLEAN_PATH="vamplean/.lake/build/lib/lean"
export LEAN_BINARY="requirements/elan/toolchains/leanprover--lean4---v4.31.0/bin/lean"
export VAMPIRE_BINARY="vampire/build/vampire"
export TPTP="$HOME/TPTP-v9.2.1"
```

```
VaLeaDate 0.1.0
Usage: VaLeaDate [options] <input_proof>

  <input_proof>            input TPTP file
  -o, --output <value>     output file for Graphviz DOT (optional)
  -i, --input-problem <value>
                           input TPTP problem file
  --v                      enable verbose output
  --vv                     enable very verbose output
  -l, --lean-binary <value>
                           path to the Lean binary
  -V, --vampire-binary <value>
                           path to the Vampire binary
  -L, --lean-library-path <value>
                           path to the Lean library
  -n, --no-lean-check      disable verification with Lean
  -p, --lean-output-path <value>
                           path to write Lean output file (optional)
  --assume-thm             assume all formulas without status are theorems
  --negc-as-thm            treat negated conjecture formulas as theorems
  --allow-axiom-mismatch   allow syntactic mismatch of axioms between proof and input problem
  --tptp-directory <value>
                           path to the TPTP directory
  -t, --timeout <value>    timeout in seconds (default: 30)
  -j, --parallel <value>   number of parallel processes for theorem checking (default: 8)
  --accept-new-symbols     accept new symbols in the input problem that are not in the proof
  --multiple-lean-files    compile Lean output with multiple files instead of one file
  --batch-size <value>     batch size (in kiB) for Lean files when compiling with multiple files
  --auto-switch-to-multi-threshold <value>
                           if the combined size (in kiB) of theorem outputs exceeds this threshold, automatically switch to compiling with multiple Lean files (default: batch-size*parallel)
  --help                   print this help message
```

## Building a starexec image

To build a starexec image (on modern hardware) vampire and valeadate are compiled statically.

This requires that the build environment has access to `gcc-musl` as well as a zlib for `musl` (possibly needs to be compiled manually depending on the used distribution).

If the requirements are met (normal build + musl), the script `./createStarExecPackage` should automatically build a suitable file. 
