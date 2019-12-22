{ pkgs ? import <nixpkgs> { } }:
let
  updateCabal = pkgs.writers.writeBashBin "update-cabal" ''
    echo "# created by cabal2nix " > ${toString ./.}/current-project.nix
    ${pkgs.cabal2nix}/bin/cabal2nix ${toString ./.} >> ${
      toString ./.
    }/current-project.nix
  '';

  run = pkgs.writers.writeBashBin "run" ''
    cd ${toString ./.}
    ${pkgs.cabal-install}/bin/cabal run site -- clean
    ${pkgs.cabal-install}/bin/cabal run site -- watch
  '';

  deploy = pkgs.writers.writeBashBin "deploy" ''
    cd ${toString ./.}
    ${pkgs.cabal-install}/bin/cabal run site -- clean
    ${pkgs.cabal-install}/bin/cabal run site -- build
    ${pkgs.cabal-install}/bin/cabal run site -- deploy
  '';
in pkgs.mkShell { buildInputs = with pkgs; [ updateCabal run deploy lessc ]; }

