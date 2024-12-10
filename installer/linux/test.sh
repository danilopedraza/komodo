#!/bin/sh

set -e

curl --proto '=https' --tlsv1.2 -sSf https://komodo-lang.org/install.sh | sh

if [ -f /usr/local/bin/komodo ]
then
    echo "✅ the binary has been moved successfully."
else
    echo "❌ the binary was not moved to /usr/bin/local."
    exit 1
fi

if [ -x /usr/local/bin/komodo ]
then
    echo "✅ the binary is executable."
else
    echo "❌ the binary is not executable."
    exit 1
fi
