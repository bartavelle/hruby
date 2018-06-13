{ mkDerivation, aeson, attoparsec, base, bytestring, Cabal, process
, QuickCheck, ruby, scientific, stdenv, stm, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "hruby";
  version = "0.3.5.2";
  src = ./.;
  setupHaskellDepends = [ base Cabal process ];
  libraryHaskellDepends = [
    aeson attoparsec base bytestring scientific stm text
    unordered-containers vector
  ];
  librarySystemDepends = [ ruby ];
  testHaskellDepends = [
    aeson attoparsec base QuickCheck text vector
  ];
  description = "Embed a Ruby intepreter in your Haskell program !";
  license = stdenv.lib.licenses.bsd3;
}
