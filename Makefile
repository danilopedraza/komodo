.ONESHELL:

CURRENT_DIR = $(shell pwd)
WEBSITE_DIR = $(CURRENT_DIR)/site

build: build-site build-playground build-book

build-core-windows-amd64:
	cd $(CURRENT_DIR)/core
	nix build '.#cross-x86_64-windows'

build-core-linux-amd64:
	cd $(CURRENT_DIR)/core
	nix build '.#cross-x86_64-linux'

build-core-linux-arm64:
	cd $(CURRENT_DIR)/core
	nix build '.#cross-aarch64-linux'

build-core-wasm:
	cd $(CURRENT_DIR)/core
	cargo build --target wasm32-unknown-unknown

build-site:
	mkdir -p $(WEBSITE_DIR)
	cp $(CURRENT_DIR)/website/index.html $(CURRENT_DIR)/website/index.css $(WEBSITE_DIR)

build-playground:
	mkdir -p $(WEBSITE_DIR)/play
	(cd $(CURRENT_DIR)/core-browser && sh build.sh $(WEBSITE_DIR)/play)

build-book:
	mdbook build $(CURRENT_DIR)/book --dest-dir $(WEBSITE_DIR)/book

deploy-vsc-ext:
	cd $(CURRENT_DIR)/vsc-extension
	vsce publish --skip-duplicate
	npx --yes ovsx publish --skip-duplicate

test-core:
	cd $(CURRENT_DIR)/core
	sh test.sh $(CURRENT_DIR)/examples

test-core-browser:
	cd $(CURRENT_DIR)/core-browser
	cargo fmt --check
	cargo clippy --all-targets --all-features
	cargo test --all-features -- --test-threads=1
	wasm-pack build --target web --out-dir komodo --dev

lint-core:
	sh lint-rust.sh $(CURRENT_DIR)/core/Cargo.toml

lint-core-browser:
	sh lint-rust.sh $(CURRENT_DIR)/core-browser/Cargo.toml

serve-book:
	mdbook serve $(CURRENT_DIR)/book --open

run-repl:
	cd core
	cargo run --all-features --quiet
