{
  buildCargoPackage,
  diffutils,
  gnum4,
  gnumake,
  libgcc,
}:

buildCargoPackage {
  src = ./.;
  
  nativeBuildInputs = [
    # These dependencies are necessary to build Rug,
    # which (indirectly) recompiles GMP and MPFR...
    diffutils
    gnum4
    gnumake
    libgcc
  ];

  cargoBuildOptions = default: default ++ [ "--all-features" ];
}
