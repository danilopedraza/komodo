#!/bin/bash

case $1 in
  test-core)
    (cd core; sh test.sh)
    ;;
esac
