let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs { };
  naersk = pkgs.callPackage (fetchTarball "https://github.com/nix-community/naersk/archive/refs/heads/master.zip") { };
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
