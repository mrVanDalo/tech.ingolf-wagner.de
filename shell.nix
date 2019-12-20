{ pkgs ? import <nixpkgs> { } }:
let
  updateCabal = pkgs.writeShellScriptBin "update-cabal" # sh
    ''
      echo "# created by cabal2nix " > ${toString ./.}/current-project.nix
      ${pkgs.cabal2nix}/bin/cabal2nix ${toString ./.} >> ${
        toString ./.
      }/current-project.nix
    '';
in pkgs.mkShell { buildInputs = with pkgs; [ updateCabal ]; }

