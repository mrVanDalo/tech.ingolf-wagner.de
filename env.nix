{ pkgs ? import <nixpkgs> {
  overlays = [
    (self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = self: super: {
          datetime = super.callPackage ./datetime.nix { };
          current-project = super.callPackage ./current-project.nix { };
        };
      };
    })
  ];
} }:
pkgs.haskellPackages.current-project.env
