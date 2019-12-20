{ pkgs ? import <nixpkgs> { } }:
let
  updateCabal = pkgs.writers.writeBashBin "update-cabal"
    ''
      echo "# created by cabal2nix " > ${toString ./.}/current-project.nix
      ${pkgs.cabal2nix}/bin/cabal2nix ${toString ./.} >> ${
        toString ./.
      }/current-project.nix
    '';

    run = pkgs.writers.writeBashBin "run"
    ''
      ${pkgs.cabal-install}/bin/cabal run site -- watch
      '';
in pkgs.mkShell { buildInputs = with pkgs; [ updateCabal run ]; }

