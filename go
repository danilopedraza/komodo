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
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      POSITIONAL_ARG=$1 # save positional arg
      shift # past argument
      ;;
  esac
done

case "$POSITIONAL_ARG" in
  test-core)
    (cd core; sh test.sh)
    ;;
  lint-core)
    (cd core; sh lint.sh)
    ;;
  deploy-book)
    
    ;;
esac
