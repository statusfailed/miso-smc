{ mkDerivation, aeson, base, bimap, containers
, hypergraph-rewriting, miso, stdenv
}:
mkDerivation {
  pname = "miso-smc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bimap containers hypergraph-rewriting miso
  ];
  description = "miso-smc";
  license = stdenv.lib.licenses.mit;
}
