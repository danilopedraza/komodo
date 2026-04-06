#!/bin/sh

set -e

sh /install.sh

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

someone_failed=false
for module in utils
do
    if [ ! -f "/usr/local/lib/komodo/$module.komodo" ] ; then
        echo "❌ the module '$module.komodo' is not in /usr/local/lib/komodo/."
        someone_failed=true
    fi
done

if [ "$someone_failed" = true ] ; then
    echo "The standard library is incomplete."
    exit 1
fi


echo "✅ The standard library has been installed successfully."

output=$(eval "komodo /hello.komodo $file" 2>&1)

if [ $? -ne 0 ]; then
    echo "❌ The interpreter failed executing `examples/hello.komodo`" >&2
    echo "This is the message from the interpreter:" >&2
    echo "$output" >&2
fi

echo "✅ The interpreter works with a basic program."
