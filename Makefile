.ONESHELL:

ROOT_DIR = $(shell pwd)
WEBSITE_DIR = $(ROOT_DIR)/site

build: build-site build-book build-installer build-report

build-installer:
	cp $(ROOT_DIR)/installer/linux/install.sh $(WEBSITE_DIR)

build-core-wasm:
	cd $(ROOT_DIR)/core
	cargo build --target wasm32-unknown-unknown

build-report:
	nix-build -A report && cp $(PWD)/result/report.pdf $(WEBSITE_DIR)

build-site:
	mkdir -p $(WEBSITE_DIR)
	cp $(ROOT_DIR)/website/index.html $(ROOT_DIR)/website/index.css $(WEBSITE_DIR)

build-playground:
	mkdir -p $(WEBSITE_DIR)/play
	(cd $(ROOT_DIR)/core-browser && sh build.sh $(WEBSITE_DIR)/play)

build-book:
	nix-build -A book && cp -r $(PWD)/result/book $(PWD)/site/book

test: test-core

test-core:
	cd $(ROOT_DIR)/core
	KOMODO_STD=$(ROOT_DIR)/std/ sh test.sh $(ROOT_DIR)/examples

test-core-browser:
	cd $(ROOT_DIR)/core-browser
	cargo fmt --check
	cargo clippy --all-targets --all-features
	cargo test --all-features -- --test-threads=1
	wasm-pack build --target web --out-dir komodo --dev

test-installer:
	docker run --rm  \
		-v $(ROOT_DIR)/installer/linux/identity_script:/usr/bin/sudo \
		-v $(ROOT_DIR)/installer/linux/install.sh:/install.sh \
		-v $(ROOT_DIR)/installer/linux/test.sh:/test.sh alpine/curl:8.9.1 \
		sh "/test.sh"

lint-core:
	sh lint-rust.sh $(ROOT_DIR)/core/Cargo.toml

lint-core-browser:
	sh lint-rust.sh $(ROOT_DIR)/core-browser/Cargo.toml

serve-book:
	mdbook serve $(ROOT_DIR)/book --open

repl:
	cd core
	KOMODO_STD=$(ROOT_DIR)/std/ cargo run --all-features --quiet

env:
	nix-shell
