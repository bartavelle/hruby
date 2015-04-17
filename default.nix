{ mkDerivation, aeson, attoparsec, base, bytestring, QuickCheck
, scientific, stdenv, stm, text, unordered-containers, vector, ruby_2_1
}:
mkDerivation {
  pname = "hruby";
  version = "0.3.1";
  sha256 = "1w13j70r6b66nyjj2fsa1z1m5p8v0zil6jf31x0h0f222x4fvmih";
  buildDepends = [
    aeson attoparsec base bytestring scientific stm text
    unordered-containers vector ruby_2_1
  ];
  testDepends = [ aeson attoparsec base QuickCheck text vector ];
  description = "Embed a Ruby intepreter in your Haskell program !";
  license = stdenv.lib.licenses.bsd3;
}
