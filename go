#!/bin/bash
set -e

mode=$1 # dev or prod

case $2 in
  test-core)
    (cd core; sh test.sh $mode)
    ;;
esac
