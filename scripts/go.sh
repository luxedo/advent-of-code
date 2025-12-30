#!/bin/bash

set -e

TOOL=$1
shift

DIRS=$(echo "$@" | xargs -n1 dirname | sort -u)

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
fmt)
  echo "$@" | rg "\.go$" | xargs -n1 go fmt
  ;;
lint)
  run_tool "golangci-lint run --allow-parallel-runners"
  ;;
*)
  echo "Unknown tool: $TOOL"
  exit 1
  ;;
esac
