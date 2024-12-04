#!/bin/sh

set -e
echo "Installing Komodo... (this will require root access)"

cd /tmp
wget https://github.com/danilopedraza/komodo/releases/download/v0.1.0/komodo
sudo chmod +x komodo && mv komodo /usr/local/bin/komodo

echo "Komodo is now installed! Try the REPL by typing 'komodo'"
