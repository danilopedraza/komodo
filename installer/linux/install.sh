#!/bin/sh

set -e
echo "Installing Komodo... (this will require root access)"

cd /tmp
wget https://github.com/danilopedraza/komodo/releases/download/v0.2.0/komodo
sudo chmod +x komodo
sudo mv komodo /usr/local/bin/komodo

echo "Installing the standard library..."
sudo mkdir -p /usr/local/lib/komodo

for module in utils
do
    wget https://github.com/danilopedraza/komodo/releases/download/v0.2.0/$module.komodo
    sudo mv $module.komodo /usr/local/lib/komodo/$module.komodo
done

echo "Komodo is now installed! Try the REPL by typing 'komodo'"
