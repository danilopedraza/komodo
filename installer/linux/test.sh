#!/bin/sh

set -e

curl --proto '=https' --tlsv1.2 -sSf https://komodo-lang.org/install.sh | sh

if [ -f /usr/local/bin/komodo ]
then
    echo "✅ the binary has been moved to /usr/local/bin/ successfully."
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

declare -a modules=("utils")

for module in "${modules[@]}"
do
    if [ ! -f "/usr/local/lib/komodo/$module.komodo" ]
    then
        echo "❌ the module '$module.komodo' is not in /usr/local/lib/komodo/."
        echo "The standard library is incomplete."
        exit 1
    fi
done

echo "✅ The standard library has been installed successfully."
