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

  # See full reference at https://devenv.sh/reference/options/
}
