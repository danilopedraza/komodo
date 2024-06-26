{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.cargo
    pkgs.git
    pkgs.mdbook
    pkgs.nodejs_20
  ];

  # See full reference at https://devenv.sh/reference/options/
}
