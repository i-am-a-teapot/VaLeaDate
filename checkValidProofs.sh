#!/bin/zsh

export VAMPLEAN_PATH=/home/jonas/VampireBenchmarking/vamp_lean/VampLean 
export VAMPIRE_BINARY=/home/jonas/vampire/build/vampire 
export LEAN_BINARY=/home/jonas/.elan/toolchains/leanprover--lean4---v4.29.0/bin/lean
export TPTP=/home/jonas/TPTP-v9.2.1
for proof in testVampireProofs/postprocessed_cnf/*.p.log; do
  echo "Running good proof: $proof"
  time ./valeadate "$proof" --assume-thm --allow-axiom-mismatch --negc-as-thm -t 10
done