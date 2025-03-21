let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs { config = {}; overlays = []; };
in
{
  book = pkgs.callPackage ./book/book.nix { };
  report = pkgs.callPackage ./report/report.nix {
    buildTypstDocument = pkgs.callPackage ./report/build-typst-document.nix { };
  };
}
