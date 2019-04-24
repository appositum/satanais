{ mkDerivation, base, megaparsec, parser-combinators, scientific
, stdenv
}:
mkDerivation {
  pname = "satanais";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base megaparsec parser-combinators scientific
  ];
  executableHaskellDepends = [
    base megaparsec parser-combinators scientific
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/appositum/satanais#readme";
  license = stdenv.lib.licenses.asl20;
}
