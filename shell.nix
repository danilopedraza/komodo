{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/24.05.tar.gz";
    sha256 = "1lr1h35prqkd1mkmzriwlpvxcb34kmhc9dnr48gkm8hh089hifmx";
  }) {} }:

pkgs.mkShell {
  packages = [
    pkgs.bashInteractive
    pkgs.mdbook
    pkgs.nodejs_20
    pkgs.rustup
    pkgs.typst
    pkgs.wasm-bindgen-cli
    pkgs.wasm-pack
    pkgs.vsce
  ];

  shellHook = ''
    rustup default stable
  '';
}
