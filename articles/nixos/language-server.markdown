---
title: Language Server
date: 2019-09-11
info: How to setup your projects using lsp, emacs (or your favorite editor), direnv and nix-shell.
---

# Setup

In this article we are discussing a concrete setup with concrete tools and a concrete language.
But the solutions described here, are intended to help you with your set up.

Here are the Tools used.

* [Spacemacs](http://spacemacs.org/) : my editor or choice.
* [all-hies](https://github.com/Infinisil/all-hies) : to start a [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine) which is the haskell-lsp-server.
* [direnv](https://direnv.net/) : to automatically load `shell.nix` configuration in my editor, when opening a file.
* [lsp-haskell.el](https://github.com/emacs-lsp/lsp-haskell) : the emacs plugin to interact with the haskell-ide-engine.
* [nix-shell](https://nixos.wiki/wiki/Development_environment_with_nix-shell) : because all projects should have one.

## Goal

* Configure Spacemacs as much as possible via `configuration.nix`, without the `lsp-server` being configured by the `configuration.nix`.
* The `lsp-server` setup should be fully defined inside the `shell.nix` of the project I'm working on.

This way project specific tweaks are stored in the place where it belongs,
and other people can use their favorite IDE with the same setup.

# Configure Spacemacs

Spacemacs is basically an `~/.emacs.d` folder and a mutable file `~/.spacemacs`.
I tried to configure `~/.spacemacs` via [home-manager](https://github.com/rycee/home-manager)
but this does not play well with updates and with `customization`.

Now I'm using [home-manager](https://github.com/rycee/home-manager)
to configure files in `~/.spacemacs.d/` and `load` them in 
the configuration functions inside `~/.spacemacs`. A simple `(load "~/.spacemacs.d/hook-user-config.el")`
inside the `dotspacemacs/user-config` function is enough, to make it work.

```nix
{ pkgs, lib, config, ... }:
let

  user = "mainUser";
  userName = config.users.users."${user}".name;
  home = config.users.users."${user}".home;
  fontSize = 14;

  startupBanner = pkgs.fetchurl{
    url = "https://github.com/NixOS/nixos-homepage/raw/master/logo/nix-wiki.png";
    sha256 = "1hrz7wr7i0b2bips60ygacbkmdzv466lsbxi22hycg42kv4m0173";
  };

in
{

  systemd.services =
    let
      clone =
        repository: folder: branch:
        {
          enable = true;
          wantedBy = [ "multi-user.target" ];
          description = "clone ${repository} to ${folder}";
          serviceConfig.User = userName;
          unitConfig.ConditionPathExists = "!${folder}";
          script = ''
            ${pkgs.git}/bin/git clone ${repository} --branch ${branch} ${folder}
          '';
        };
    in
    {
      emacs-pull = clone "https://github.com/syl20bnr/spacemacs" "${home}/.emacs.d" "master";
    };

  home-manager.users."${user}" = {

      home.file.".spacemacs.d/hook-init.el".text = ''
        ;; just add (load "~/.spacemacs.d/hook-init.el")
        ;; at the end of dotspacemacs/init function

        ;; overrides of dotspacemacs/init ()
        (setq-default
         dotspacemacs-themes '(solarized-light solarized-dark)
         dotspacemacs-startup-banner "${startupBanner}"
         dotspacemacs-default-font '("Terminus"
                                     :size ${toString fontSize}
                                     :weight normal
                                     :width normal
                                     :powerline-scale 1.1))
      '';

      home.file.".spacemacs.d/hook-layers.el".text = ''
        ;; just add (load "~/.spacemacs.d/hook-layers.el") 
        ;; at the end of dotspacemacs/layers function

        (let
          ((user-layers dotspacemacs-configuration-layers))
          (setq
            dotspacemacs-configuration-layers
              (append user-layers
                '( spell-checking
                   syntax-checking
                   (haskell :variables
                      haskell-enable-hindent t
                      haskell-completion-backend 'lsp
                      haskell-enable-hindent-style "gibiansky"
                      haskell-process-type 'cabal-new-repl)
                 ))))

        (let
          ((user-packages dotspacemacs-additional-packages ))
          (setq
            dotspacemacs-additional-packages
              (append user-packages
                '( lsp-mode
                   lsp-ui
                   lsp-haskell
                   direnv
                ))))
      '';

      home.file.".spacemacs.d/hook-user-config.el".text = ''
        ;; just add (load "~/.spacemacs.d/hook-user-config.el") 
        ;; at the end of dotspacemacs/user-config function

        ;; lsp setup for haskell
        ;; hie-wrapper must be installed and configured in the direnv setup
        (setq lsp-haskell-process-path-hie "hie-wrapper")
        (setq lsp-response-timeout 60)
        (require 'lsp-haskell)
        (add-hook 'haskell-mode-hook #'lsp)
        (add-hook 'haskell-mode-hook #'direnv-update-environment)
      '';

    };
}
```

We setup emacs to run `direnve-update-environment` and `lsp` once we start the `haskell-mode`.
But we did not install `lsp`.
In my setups the `lsp-server` is installed by the project file (lsp.nix), and is loaded via `direnv` (`direnv-update-environment` in emacs).
If you don't like that just use the snippet from the next section.

## Alternative Configuration (install lsp in the configuration.nix)

You can install the `lsp` (in our case `hie-wrapper`) globally in your `configuration.nix` .
I usually do this in my projects (via `lsp.nix`). Here is the part that differs.

```nix
   home.file.".spacemacs.d/hook-user-config.el".text =
     let
       all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
     in ''
        ;; just add (load "~/.spacemacs.d/hook-user-config.el") 
        ;; at the end of dotspacemacs/user-config function

        ;; lsp setup for haskell
        (setq lsp-haskell-process-path-hie 
          "${all-hies.selection{ selector = p: { inherit (p) ghc864;}; } }/bin/hie-wrapper")
        (setq lsp-response-timeout 60)
        (require 'lsp-haskell)
        (add-hook 'haskell-mode-hook #'lsp)
        (add-hook 'haskell-mode-hook #'direnv-update-environment) ;; still needed
      '';
```

# Setup the project

For a Haskell project I have this minimal setup of files.

## lsp.nix

This file is to setup the `lsp-server`.
If you already installed the `lsp-server` via the `configuration.nix`, this file is not necessary,
but also does not hurt.

```nix
{ pkgs ?  import <nixpkgs> {} }:
let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellPackages.hoogle
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    (all-hies.selection { selector = p: {inherit (p) ghc864; }; })
  ];
}
```

## env.nix 

Provides the environment to run
`cabal test` and `cabal build`.
All package files (e.g. `./current-project.nix`) are created by `cabal2nix`.

```nix
{ pkgs ?  import <nixpkgs> {
  overlays = [
    (self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = self: super: {
          datetime = super.callPackage ./datetime.nix {};
          current-project = super.callPackage ./current-project.nix { };
        };
      };
    })];
}}:
pkgs.haskellPackages.current-project.env
```

## `shell.nix`

For other scripts and tooling important for development.

```nix
{ pkgs ?  import <nixpkgs> {} }:
let
  updateCabal = pkgs.writeShellScriptBin "update-cabal" /* sh */ ''
    echo "# created by cabal2nix " > ${toString ./.}/current-project.nix
    ${pkgs.cabal2nix}/bin/cabal2nix ${toString ./.} >> ${toString ./.}/current-project.nix
  '';
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    updateCabal
    openapi-generator-cli
    openssl
    cabal2nix
  ];
}
```

## `.envrc`

finally we need a `direnv` configuration file.
`direnv` and the `direnv-mode` make it possible
to load the environment needed and provided by the `*.nix` files.

```sh
use nix ./env.nix
use nix ./lsp.nix
use nix ./shell.nix
```
Don't forget to run `direnv allowed . ` in the project folder.

# Conclusion

Now we are capable to use  the `lsp-server` configured in all our projects, 
with the editor we prefer.
Your colleagues will have little problems with the setup and improve it.

If you prefer to install the lsp globally, you can simply do that as described,
this will not interfere with the `lsp` server setup in the `lsp.nix` file.

I'm running this setup for quite a while now, and 
I experience little to no problems with it.
The most common thing is that I have to fire `lsp-restart-workspace` to remove old errors,
but doing this every hour is not a problem for me.

## Support

If you have comments or problems just ping me `palo @ irc.freenode.net`
