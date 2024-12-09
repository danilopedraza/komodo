#!/bin/sh

set -e

curl --proto '=https' --tlsv1.2 -sSf https://komodo-lang.org/install.sh | sh
test -f /usr/local/bin/komodo
