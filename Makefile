.ONESHELL:

CURRENT_DIR = $(shell pwd)
WEBSITE_DIR = $(CURRENT_DIR)/site

build: build-site build-book build-installer build-report

build-installer:
	cp $(CURRENT_DIR)/installer/linux/install.sh $(WEBSITE_DIR)

build-core-wasm:
	cd $(CURRENT_DIR)/core
	cargo build --target wasm32-unknown-unknown

build-report:
	typst compile $(CURRENT_DIR)/report/report.typ $(WEBSITE_DIR)/report.pdf

build-site:
	mkdir -p $(WEBSITE_DIR)
	cp $(CURRENT_DIR)/website/index.html $(CURRENT_DIR)/website/index.css $(WEBSITE_DIR)

build-playground:
	mkdir -p $(WEBSITE_DIR)/play
	(cd $(CURRENT_DIR)/core-browser && sh build.sh $(WEBSITE_DIR)/play)

build-book:
	nix-build -A book && cp -r $(PWD)/result/book $(PWD)/site/book

test: test-core

test-core:
	cd $(CURRENT_DIR)/core
	KOMODO_STD=$(CURRENT_DIR)/std/ sh test.sh $(CURRENT_DIR)/examples

test-core-browser:
	cd $(CURRENT_DIR)/core-browser
	cargo fmt --check
	cargo clippy --all-targets --all-features
	cargo test --all-features -- --test-threads=1
	wasm-pack build --target web --out-dir komodo --dev

test-installer:
	docker run --rm  \
		-v $(CURRENT_DIR)/installer/linux/identity_script:/usr/bin/sudo \
		-v $(CURRENT_DIR)/installer/linux/install.sh:/install.sh \
		-v $(CURRENT_DIR)/installer/linux/test.sh:/test.sh alpine/curl:8.9.1 \
		sh "/test.sh"

lint-core:
	sh lint-rust.sh $(CURRENT_DIR)/core/Cargo.toml

lint-core-browser:
	sh lint-rust.sh $(CURRENT_DIR)/core-browser/Cargo.toml

serve-book:
	mdbook serve $(CURRENT_DIR)/book --open

repl:
	cd core
	KOMODO_STD=$(CURRENT_DIR)/std/ cargo run --all-features --quiet

env:
	nix-shell
