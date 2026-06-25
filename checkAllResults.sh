#!/bin/zsh

export VAMPLEAN_PATH=/home/jonas/VampireBenchmarking/vamp_lean/VampLean 
export VAMPIRE_BINARY=/home/jonas/vampire/build/vampire 
export LEAN_BINARY=/home/jonas/.elan/toolchains/leanprover--lean4---v4.29.0/bin/lean

#echo "Running good proofs:"
time ./valeadate Problems/Good/example1_c_proof.p
time ./valeadate Problems/Good/example2_c_proof.p
time ./valeadate Problems/Good/example3_c_proof.p
time ./valeadate Problems/Good/PUZ001_e.p
time ./valeadate Problems/Good/PUZ001_iProver.p
#
#echo "Running bad proofs:"
time ./valeadate Problems/Bad/example1_e_proof.p
time ./valeadate Problems/Bad/example2_e_proof.p
time ./valeadate Problems/Bad/example2p_e_proof.p
time ./valeadate Problems/Bad/example3_e_proof.p
time ./valeadate Problems/Bad/example4_e_proof.p
