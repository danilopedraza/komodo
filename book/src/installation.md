# Installation

Currently, you can install Komodo downloading binaries or building from source.

## Download binaries

There are pre-compiled binaries for x86-64 Linux [here](https://github.com/danilopedraza/komodo/releases/download/v0.4.1/komodo). If this is not your architecture, you will have to build from source.

You can use this to install Komodo for all users (requires root access):

```
curl --proto '=https' --tlsv1.2 -sSf https://komodo-lang.org/install.sh | sh
```

You can also install Komodo (only for your user) with these commands:

```
wget https://github.com/danilopedraza/komodo/releases/download/v0.4.1/komodo
chmod +x komodo
mv komodo $HOME/.local/bin
```

## Build from source

You can build and install Komodo using the Rust infrastructure. Komodo is not published in [crates.io](https://crates.io/), so you will have to clone the GitHub repository. To run the following commands you must have [Rust](https://www.rust-lang.org/learn/get-started) installed.

```
git clone https://github.com/danilopedraza/komodo.git
cd komodo/core
cargo build --release --all-features
chmod +x target/release/komodo
cp target/release/komodo $HOME/.local/bin
```

Now you should be able to use Komodo from the command line with the `komodo` command.

Alternatively, you can try Komodo by running `cargo run --all-features` in the `komodo/core` directory.
