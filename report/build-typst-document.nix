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

# Use an explicit package path: macOS does not use XDG_DATA_HOME for packages.
let
  typstDependencyScript = dep: ''
    mkdir -p "$TYPST_PACKAGE_PATH/preview/${dep.name}"
    ln -s ${packagesRepo}/packages/preview/${dep.name}/${dep.version} "$TYPST_PACKAGE_PATH/preview/${dep.name}/${dep.version}"
  '';
  typstDependencyScripts = lib.concatStringsSep "\n" (builtins.map typstDependencyScript typstDependencies);
in
stdenv.mkDerivation {
  inherit name src;
  nativeBuildInputs = [
    typst
  ];

  configurePhase = ''
    runHook preConfigure
    export TYPST_PACKAGE_PATH="$TMPDIR/typst-packages"
    export TYPST_PACKAGE_CACHE_PATH="$TMPDIR/typst-package-cache"
    ${typstDependencyScripts}
    runHook postConfigure
  '';
}
