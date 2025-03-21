# Original from https://github.com/NorfairKing/typst.nix
{
  lib,
  stdenv,
  typst,
}:
{
  src,
  typstDependencies,
  packagesRepo,
  name,
  ...
}:

# We try to use this trick to make packages available locally:
let
  typstDependencyScript = dep: ''
    mkdir -p $XDG_DATA_HOME/typst/packages/preview/${dep.name}
    ln -s ${packagesRepo}/packages/preview/${dep.name}/${dep.version} $XDG_DATA_HOME/typst/packages/preview/${dep.name}/${dep.version}
  '';
  typstDependencyScripts = lib.concatStringsSep "\n" (builtins.map typstDependencyScript typstDependencies);
in
stdenv.mkDerivation {
  inherit name src;
  buildInputs = [
    typst
  ];

  configurePhase = ''
    export XDG_DATA_HOME=$(pwd)
    ${typstDependencyScripts}
  '';
}
