let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs { };
in

pkgs.mkShell {
  packages = with pkgs; [
    bashInteractive
    mdbook
    nil
    nodejs_20
    rustup
    typst
    tinymist
    wasm-bindgen-cli
    wasm-pack
    vsce
  ];

  shellHook = ''
    rustup default stable
  '';
}
