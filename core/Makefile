CURRENT_DIR = $(shell pwd)

build:
	cargo build --all-features --release

test:
	KOMODO_STD=$(CURRENT_DIR)/../std/ sh test.sh $(CURRENT_DIR)/../examples

bench:
	echo "Remember to set `second_to_last_push_hash` to the hash of the commit that you want to compare to HEAD."
	KOMODO_STD=$(CURRENT_DIR)/../std/ sh bench.sh $(CURRENT_DIR)/../bench/ $(second_to_last_push_hash)

lint:
	cargo clippy --all-targets --all-features

fmt:
	cargo fmt --check

repl:
	KOMODO_STD=$(CURRENT_DIR)/../std/ cargo run --all-features --quiet
