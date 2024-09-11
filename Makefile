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
	sh test.sh

test-core-browser:
	cd $(CURRENT_DIR)/core-browser
	sh test.sh

lint-core:
	cd $(CURRENT_DIR)/core
	sh lint.sh

lint-core-browser:
	cd $(CURRENT_DIR)/core-browser
	sh lint.sh

serve-book:
	mdbook serve $(CURRENT_DIR)/book --open

run-repl:
	cd core
	cargo run --all-features --quiet
