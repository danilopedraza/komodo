{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  packages = [
    pkgs.mdbook
    pkgs.nodejs_20
    pkgs.rustup
    pkgs.wasm-bindgen-cli
    pkgs.wasm-pack
    pkgs.vsce
  ];

  shellHook = ''
    rustup default stable
  '';
}
