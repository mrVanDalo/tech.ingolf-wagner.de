# created by cabal2nix
{ mkDerivation, base, filepath, hakyll, pandoc, stdenv }:
mkDerivation {
  pname = "tech.ingolf-wagner.de";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath hakyll pandoc ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
