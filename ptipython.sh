#!/bin/bash

initial_dir=$(pwd)
dir="$(dirname -- "${BASH_SOURCE[0]}")"

cd $dir/Ptipython
dune build
cd ..
cd $initial_dir
$dir/Ptipython/ptipython.exe $1
