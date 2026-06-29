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
Usage: VaLeaDate [options] <input_proof>

  <input_proof>            input TPTP file
  -o, --output <value>     output file for Graphviz DOT (optional)
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
  --help                   print this help message
```