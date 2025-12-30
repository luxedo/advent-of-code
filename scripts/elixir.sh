#!/bin/bash

set -e

TOOL=$1
shift

# shellcheck disable=SC2016
DIRS=$(echo "$@" | tr ' ' '\n' | rg "\.ex$" | xargs -r -I{} -n1 sh -c 'dirname $(dirname {})' | sort -u)

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
format)
  run_tool "mix format"
  ;;
build)
  run_tool "mix compile --all-warnings --warnings-as-errors"
  ;;
*)
  echo "Unknown tool: $TOOL"
  exit 1
  ;;
esac
