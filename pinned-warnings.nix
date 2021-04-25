{ mkDerivation, base, bytestring, containers, directory, ghc, lib
}:
mkDerivation {
  pname = "pinned-warnings";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory ghc
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
