let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.11";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in
{
  book = pkgs.callPackage ./book/book.nix { };
  report = pkgs.callPackage ./report/report.nix {
    buildTypstDocument = pkgs.callPackage ./report/build-typst-document.nix { };
  };
}
