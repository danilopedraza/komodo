let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs { };
in

pkgs.mkShell {
  packages = with pkgs; [
    bashInteractive
    mdbook
    diffutils
    gnum4
    gnumake
    nil
    libgcc
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
