#!/bin/zsh

export VAMPLEAN_PATH=/home/jonas/VampireBenchmarking/vamp_lean/VampLean 
export VAMPIRE_BINARY=/home/jonas/vampire/build/vampire 
export LEAN_BINARY=/home/jonas/.elan/toolchains/leanprover--lean4---v4.29.0/bin/lean

for proof in testVampireProofs/postprocessed_cnf/*.p.log; do
  echo "Running good proof: $proof"
  time ./valeadate "$proof" --assume-thm --allow-axiom-mismatch --negc-as-thm
done