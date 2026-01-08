{
  buildTypstDocument,
}:

buildTypstDocument {
  src = ./.;
  name = "report";
  packagesRepo = builtins.fetchGit {
    url = "https://github.com/typst/packages";
    rev = "2cfc68ae23980f656c7971a53667c08e6a6c05df";
    # Use a recent typst packages commit
  };

  typstDependencies = [
    {
      name = "cetz";
      version = "0.3.4";
    }
    {
      name = "chronos";
      version = "0.2.1";
    }
    {
      name = "oxifmt";
      version = "0.2.1";
    }
    {
      name = "simplebnf";
      version = "0.1.1";
    }
    {
      name = "syntree";
      version = "0.2.1";
    }
  ];
}
