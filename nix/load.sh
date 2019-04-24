#!/bin/sh

FILE_ABS_PATH="$(readlink -f "$0")"
PROJ_NIX_PATH="$(dirname "$FILE_ABS_PATH")"

cd $PROJ_NIX_PATH
cd ../
nix-shell --run "cabal --enable-nix v1-repl lib:hope"
