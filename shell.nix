let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs { };
in

pkgs.mkShell {
  packages = with pkgs; [
    bashInteractive
    mdbook
    gnumake
    nil
    nodejs_20
    rustup
    typst
    tinymist
    wasm-bindgen-cli
    wasm-pack
    vsce
    hyperfine
  ];

  shellHook = ''
    rustup default stable
  '';
}
