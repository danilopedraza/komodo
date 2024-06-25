#!/bin/bash
set -e

case $1 in
  test-core)
    (cd core; sh test.sh)
    ;;
  lint-core)
    (cd core; sh lint.sh)
esac
