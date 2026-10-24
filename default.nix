let
  packages = (builtins.getFlake (toString ./.)).packages.${builtins.currentSystem};
in
{
  inherit (packages) book report;
  core = packages.komodo;
}
