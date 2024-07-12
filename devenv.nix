{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.git
    pkgs.mdbook
    pkgs.nodejs_20
    pkgs.rustup
    pkgs.wasm-pack
    pkgs.vsce
  ];

  enterShell = ''
    source autocomplete.sh
    rustup default stable
  '';

  # See full reference at https://devenv.sh/reference/options/
}
