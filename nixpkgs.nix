let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/25.11.tar.gz";
    sha256 = "1zn1lsafn62sz6azx6j735fh4vwwghj8cc9x91g5sx2nrg23ap9k";
  };
in
  nixpkgs
