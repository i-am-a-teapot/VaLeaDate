#!/bin/zsh

for proof in testVampireProofs/postprocessed_cnf/*.p.log; do
  echo "Running good proof: $proof"
  time ./valeadate "$proof" --assume-thm --allow-axiom-mismatch --negc-as-thm
done