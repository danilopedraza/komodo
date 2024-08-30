set -e

cp index.html index.css index.js $1

wasm-pack build --target web --out-dir komodo --release

mkdir -p "$1/komodo/"
cp komodo/komodo_browser.js komodo/komodo_browser_bg.wasm "$1/komodo"
