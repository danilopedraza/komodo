.ONESHELL:

CURRENT_DIR = $(shell pwd)
WEBSITE_DIR = $(CURRENT_DIR)/site

deploy: deploy-site deploy-playground deploy-book deploy-vsc-ext

deploy-site:
	mkdir -p $(WEBSITE_DIR)
	cp $(CURRENT_DIR)/website/index.html $(CURRENT_DIR)/website/index.css $(WEBSITE_DIR)

deploy-playground:
	mkdir -p $(WEBSITE_DIR)/play
	(cd $(CURRENT_DIR)/core-browser && sh build.sh $(WEBSITE_DIR)/play)

deploy-book:
	mdbook build $(CURRENT_DIR)/book --dest-dir $(WEBSITE_DIR)/book

deploy-vsc-ext:
	cd $(CURRENT_DIR)/vsc-extension
	vsce publish --skip-duplicate
	npx --yes ovsx publish --skip-duplicate

build-book:
	mdbook build $(CURRENT_DIR)/book

build-core-wasm:
	cd $(CURRENT_DIR)/core
	cargo build --target wasm32-unknown-unknown

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
	cargo run --all-features --quiet --manifest-path $(CURRENT_DIR)/core/Cargo.toml
