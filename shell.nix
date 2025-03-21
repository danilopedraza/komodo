let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs { };
in

pkgs.mkShell {
  packages = [
    pkgs.bashInteractive
    pkgs.mdbook
    pkgs.nil
    pkgs.nodejs_20
    pkgs.rustup
    pkgs.typst
    pkgs.tinymist
    pkgs.wasm-bindgen-cli
    pkgs.wasm-pack
    pkgs.vsce
  ];

  shellHook = ''
    rustup default stable
  '';
}
