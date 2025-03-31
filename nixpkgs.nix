let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/nixos-24.11";
    sha256 = "08pwvljh8qskw8ajggfpx4x2jc1x69jc9vqkqzx68aj66vb2rsmh";
  };
in
  nixpkgs
