#!/bin/zsh
cargo build --release

for f in tests/benchmark_test_cases/*.lox
do
  echo Running $f.
  echo "===================================================="
  ./target/release/rusty_lox $f bytecode
  echo "====================================================\n"
  echo "\n"
done
