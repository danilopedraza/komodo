let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/nixos-24.11";
    sha256 = "19q0mqqzab5n7q5smw4yryplz52n838k11afdir49db73d8qly5x";
  };
in
  nixpkgs
