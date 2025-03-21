{
  buildTypstDocument,
}:

buildTypstDocument {
  src = ./.;
  name = "report";
  packagesRepo = builtins.fetchGit {
    url = "https://github.com/typst/packages";
    rev = "d7cb4032b0e105504d5dc43a60220b132b3c83b7";
    # Use a recent typst packages commit
  };

  typstDependencies = [
    {
      name = "cetz";
      version = "0.3.1";
    }
    {
      name = "chronos";
      version = "0.2.0";
    }
    {
      name = "oxifmt";
      version = "0.2.0";
    }
    {
      name = "simplebnf";
      version = "0.1.1";
    }
    {
      name = "syntree";
      version = "0.2.0";
    }
  ];
}
