# created by cabal2nix 
{ mkDerivation, base, hakyll, stdenv }:
mkDerivation {
  pname = "techblog-new";
  version = "0.1.0.0";
  src = /home/palo/dev/techblog-new;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
