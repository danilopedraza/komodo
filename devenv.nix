{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.git
    pkgs.mdbook
    pkgs.nodejs_20
    pkgs.wasm-bindgen-cli
    pkgs.wasm-pack
    pkgs.vsce
  ];

  languages = {
    rust = {
      enable = true;
      channel = "stable";
      targets = [
        "x86_64-unknown-linux-gnu"
        "wasm32-unknown-unknown"
      ];
    };
  };

  enterShell = ''
    source autocomplete.sh
  '';

  # See full reference at https://devenv.sh/reference/options/
}
