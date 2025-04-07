let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.11.tar.gz";
    sha256 = "1gx0hihb7kcddv5h0k7dysp2xhf1ny0aalxhjbpj2lmvj7h9g80a";
  };
in
  nixpkgs
