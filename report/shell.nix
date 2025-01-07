{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/branch-off-24.11.tar.gz";
    sha256 = "1gx0hihb7kcddv5h0k7dysp2xhf1ny0aalxhjbpj2lmvj7h9g80a";
  }) {} }:

pkgs.mkShell {
  packages = [
    pkgs.typst
    pkgs.typst-lsp
  ];
}
