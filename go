#!/bin/bash
set -e

POSITIONAL_ARG=""

while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--target)
      TARGET="$2"
      shift # past argument
      shift # past value
      ;;
    -*|--*)
      echo "Unknown option: $1"
      exit 1
      ;;
    *)
      POSITIONAL_ARG=$1 # save positional arg
      shift # past argument
      ;;
  esac
done

case "$POSITIONAL_ARG" in
  build-core-wasm)
    (cd core; cargo build --target wasm32-unknown-unknown)
    ;;
  test-core)
    (cd core; sh test.sh)
    ;;
  lint-core)
    (cd core; sh lint.sh)
    ;;
  lint-core-browser)
    (cd core-browser; sh lint.sh)
    ;;
  test-core-browser)
    (cd core-browser; sh test.sh)
    ;;
  build-book)
    mdbook build ./book
    ;;
  deploy-book)
    mdbook build ./book --dest-dir "$TARGET"
    ;;
  serve-book)
    mdbook serve ./book --open
    ;;
  deploy-site)
    mdbook build ./book --dest-dir "$TARGET/book/"
    ;;
  deploy-vsc-extension)
    (
      cd vsc-extension;
      vsce publish --skip-duplicate;
      npx --yes ovsx publish --skip-duplicate
    )
    ;;
  shortlist)
    echo build-core-wasm test-core lint-core lint-core-browser test-core-browser build-book deploy-book serve-book deploy-site deploy-vsc-extension run-repl shortlist
    ;;
  run-repl)
    (cd core; cargo run --all-features --quiet)
    ;;
  *)
    echo "Unknown option: $POSITIONAL_ARG"
esac
