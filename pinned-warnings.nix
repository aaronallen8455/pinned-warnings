{ mkDerivation, base, bytestring, containers, directory, ghc, lib
, time, transformers
}:
mkDerivation {
  pname = "pinned-warnings";
  version = "0.1.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers directory ghc time transformers
  ];
  homepage = "https://github.com/aaronallen8455/pinned-warnings#readme";
  description = "Preserve warnings in a GHCi session";
  license = lib.licenses.bsd3;
}
