{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.cargo-cross
    pkgs.docker
    pkgs.git
    pkgs.mdbook
    pkgs.nodejs_20
    pkgs.rustup
    pkgs.wasm-bindgen-cli
    pkgs.wasm-pack
    pkgs.vsce
  ];

  enterShell = ''
    rustup default stable
    source autocomplete.sh
  '';

  # See full reference at https://devenv.sh/reference/options/
}
