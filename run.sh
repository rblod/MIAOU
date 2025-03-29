#!/bin/bash
set -e

echo ">>> Compiling"
make

echo ">>> Running test_output.exe"
./test_output.exe

echo ">>> Done"
