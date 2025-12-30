#!/bin/bash

set -e

TOOL=$1
shift

# shellcheck disable=SC2016
DIRS=$(echo "$@" | xargs -I {} -n1 sh -c 'dirname $(dirname {})' | sort -u)

run_tool() {
  local cmd=$1
  for dir in $DIRS; do
    echo "--- Running $TOOL in $dir ---"
    pushd "$dir" >/dev/null || exit
    eval "$cmd"
    popd >/dev/null || exit
  done
}

case $TOOL in
stylish)
  echo "$@" | xargs stylish-haskell --inplace
  ;;
hlint)
  run_tool "hlint ."
  ;;
format-cabal)
  run_tool "cabal format"
  ;;
build)
  run_tool "cabal build --ghc-option -Wall --ghc-option -Werror"
  ;;
*)
  echo "Unknown tool: $TOOL"
  exit 1
  ;;
esac
