{
  mdbook,
  stdenv,
}:

stdenv.mkDerivation {
  pname = "book";
  version = "";
  
  src = ./.;

  nativeBuildInputs = [ mdbook ];
}
