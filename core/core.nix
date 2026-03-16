{
  buildCargoPackage,
}:

buildCargoPackage {
  src = ./.;
  
  nativeBuildInputs = [];

  cargoBuildOptions = default: default ++ [ "--all-features" ];
}
