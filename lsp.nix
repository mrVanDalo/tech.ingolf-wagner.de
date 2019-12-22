{ pkgs ? import <nixpkgs> { } }:
let
  all-hies =
    import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master")
    { };
in pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellPackages.hoogle
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    #(all-hies.selection { selector = p: {inherit (p) ghc864; }; })
  ];
}
