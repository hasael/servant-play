{ mkDerivation, aeson, async, base, bytestring, containers, Decimal
, either, generic-random, hspec, hspec-wai, hspec-wai-json
, http-types, lib, postgresql-simple, QuickCheck, refined
, resource-pool, servant-server, stm, text, transformers, wai
, wai-extra, warp, yaml
}:
mkDerivation {
  pname = "servant-play";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers Decimal either postgresql-simple
    refined resource-pool servant-server stm text transformers wai
    wai-extra warp
  ];
  executableHaskellDepends = [
    aeson async base postgresql-simple servant-server wai warp yaml
  ];
  testHaskellDepends = [
    aeson async base bytestring containers generic-random hspec
    hspec-wai hspec-wai-json http-types postgresql-simple QuickCheck
    refined resource-pool servant-server stm wai wai-extra warp
  ];
  homepage = "https://github.com/hasael/servant-play#readme";
  license = lib.licenses.bsd3;
}
