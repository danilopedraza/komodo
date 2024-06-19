# Installation

Currently, you can install Symstatic downloading binaries or building from source.

## Download binaries

There are pre-compiled binaries for x86-64 Linux [here](https://github.com/danilopedraza/symstatic/releases/download/v0.0.1/symstatic). If this is not your architecture, you will have to build from source.

You can easily download Symstatic with these commands:

```
wget https://github.com/danilopedraza/symstatic/releases/download/v0.0.1/symstatic
chmod +x symstatic
mv symstatic $HOME/.local/bin
```

## Build from source

You can build and install Symstatic using the Rust infrastructure. Symstatic is not published in [crates.io](https://crates.io/), so you will have to clone the GitHub repository. To run the following commands you must have [Rust](https://www.rust-lang.org/learn/get-started) installed.

```
git clone https://github.com/danilopedraza/symstatic.git
cd symstatic/core
cargo build --release
chmod +x target/release/symstatic
cp target/release/symstatic $HOME/.local/bin
```

Now you should be able to use Symstatic from the command line with the `symstatic` command.

Alternatively, you can try Symstatic by running `cargo run` in the `symstatic/core` directory.
