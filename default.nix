let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs { };
  naersk = pkgs.callPackage (builtins.fetchGit {
    url = "https://github.com/nix-community/naersk";
    rev = "a75c0584b0d69de943babc899530e9c70c642b42";
  }) {};
in
{
  book = pkgs.callPackage ./book/book.nix { };

  core = pkgs.callPackage ./core/core.nix {
    buildCargoPackage = naersk.buildPackage;
  };

  report = pkgs.callPackage ./report/report.nix {
    buildTypstDocument = pkgs.callPackage ./report/build-typst-document.nix { };
  };
}
