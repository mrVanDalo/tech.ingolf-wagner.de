---
title: Why NixOS
date: 2019-04-26
scheme: light
qrcode-link: why-nixos-qrcode.svg
info: |
  A Presentation about NixOS which only focuses on the positive sides of NixOS.
  It should give listeners an impression on what they gain,
  when using NixOS as their Operation System.
---
# Why NixOS

NixOS is not perfect (but very very good).

Let&#39;s look at the good parts.
---

  # What is NixOS ?

* Operation System based on [Nix Package manager](https://nixos.org/nix/).
* builtin `provisioning` system.
* `x86_32`, `x86_64`, `ARM6`, `ARM7`, `ARM8`, `AARCH64` (and many more).
* `channels` ( stable, unstable, ... ).
* 2 stable releases per year.
---

  # NixOS Features

* [idempotent](https://en.wikipedia.org/wiki/Idempotence) and more.
* rollbacks (without re-provisioning).
* powerful [package system](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/nano/default.nix)
with [modding](https://nixos.org/nixpkgs/manual/#sec-overrides)
* [overlays](https://nixos.wiki/wiki/Overlays)
* no [Dependency-Hell](https://en.wikipedia.org/wiki/Dependency_hell).
* [container-system](https://nixos.org/nixos/manual/#ch-containers) ([systemd-nspawn](https://wiki.archlinux.org/index.php/Systemd-nspawn) based).
* cross-compilation (using [binfmt](https://en.wikipedia.org/wiki/Binfmt_misc)).

---
### configuration.nix Example
```nix
{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix 
    ./systemd-example.nix
  ];

  networking.hostName = &#34;mymachine&#34;;

  time.timeZone = &#34;Europe/Berlin&#34;;

  users.users.myuser = {
    extraGroups = [ &#34;wheel&#34; &#34;networkmanager&#34; ];
    isNormalUser = true;
  };

  environment.systemPackages = [ pkgs.git pkgs.vim pkgs.sl ];

  serivce.avahi.enable = true;
}
```
---
### SystemD Service Example

```nix
{ config, pkgs, ... }:
let
  myWebsite = pkgs.fetchgit {
    src = https://github.com/mrVanDalo/landingpage.git;
    rev = &#34;f67336e0cc97c8dd118d3cafb899bce7e60280e8&#34;;
    sha = &#34;03g8b6zwrg9jyvg9iipd3dfpfimvq5qn0cxa3b24skqq3ap4wcgk&#34;;
  };
in {

  users.users.mypageuser.isNormalUser = false;
  
  systemd.services.mypage = {
    enable = true;
    serviceConfig.User = config.users.users.mypageuser.name;
    wantedBy = [ &#34;multi-user.target&#34; ];
    script = /* sh */ &#39;&#39;
      cd ${myWebsite}/docs
      ${pkgs.python}/bin/python -m SimpleHTTPServer 9000
    &#39;&#39;;
  }

  networking.firewall.allowedTCPPorts = [ 9000 ];

}
```
---

  # Nix-Shell

* shell *(bash)*  with modified run-time environment.
* software is **not** installed &#34;system-wide&#34;

--

`gimp` only in a shell
```
nix-shell -p gimp
```

---
## shell.nix example

```nix
{ pkgs ? import &lt;nixpkgs&gt; {} }:
let
  buildScript = pkgs.writers.writeDashBin &#34;build&#34; /* sh */ &#39;&#39;
    ${pkgs.lessc}/bin/lessc \
      ${toString ./src/lessc/main.less} \
      ${toString ./assets/generated/main.css}
  &#39;&#39;;
in pkgs.mkShell {
  buildInputs = [
    pkgs.elmPackages.elm
    pkgs.vscode
    buildScript
  ];

  shellHook = &#39;&#39;
    HISTFILE=${toString ./.history}
  &#39;&#39;;
}
```
---

  # NixOS Generators

community driven [tool](https://github.com/nix-community/nixos-generators)
to create various images.

```nix
{
  services.sshd.enable = true;
  users.users.root.openssh.authorizedKeys.keyFiles = ./sshKeys;
  services.home-assistant.enable = true;
}
```
--
build and run in VM
```sh
nixos-generate --configuration ./config.nix \
  --format vm --run 
```
--
build sd card for Raspberry PI
```sh
nixos-generate --configuration ./config.nix \
  --format sd-aarch64 --system aarch64-linux 
```
---

  # NixOS Containers

* *sub*configuration.nix
* mount folders
* creates a [systemd-nspawn](https://www.freedesktop.org/software/systemd/man/systemd-nspawn.html) container

--

## Main use-case

multiple instances of same service *(costs almost no overhead)*.
---

  ### NixOS Container Example

```nix
# creates a systemd service container@database96
containers.database96.config = { config, pkgs, ... }:
{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_9_6;
    port = 5432;
  }
};

# creates a systemd service container@database10
containers.database10.config = { config, pkgs, ... }:
{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_10;
    port = 5433;
  }
};
```
---

  # There is MUUCH moar!

* [Home-Manager](https://github.com/rycee/home-manager) get rid of  `.dot_file` mess.
* [NixOS-Shell](https://github.com/chrisfarms/nixos-shell) like `nix-shell` + NixOS modules.
* DevOps Tools : [NixOps](https://nixos.org/nixops/) and [krops](https://cgit.krebsco.de/krops/about/).
* [nix-review](https://github.com/Mic92/nix-review) pull-request review tool.
* [disko](https://cgit.krebsco.de/disko/about/) hard-drive setup.
* [simple-nixos-mailserver](https://gitlab.com/simple-nixos-mailserver/nixos-mailserver) a mailserver module.
* [TerraNix](https://github.com/mrVanDalo/terranix) generate [terraform](https://www.terraform.io/) configuration.
* [cabal2nix](https://github.com/NixOS/cabal2nix), [pypi2nix](https://github.com/garbas/pypi2nix), [elm2nix](https://github.com/hercules-ci/elm2nix) create `shell.nix` for programming languages.
---

  # How to start?

You liked what you saw?
Can&#39;t wait to try it out?

## Virtual Machine

* download [VirtualBox Image](https://nixos.org/nixos/download.html) 
* edit `/etc/nixos/configuration.nix`
* run `nixos-rebuild switch`
* get a feeling for it
* install a computer with your `/etc/nixos/configuration.nix`
---
## Keep in touch

* irc.freenode.net 
  * `#nixos`
  * `#nixos-de`
  * `#nixos-fr` ... 
* [Discourse](https://discourse.nixos.org/)
* [NixOS weekly](https://weekly.nixos.org/)
* [Github](https://github.com/nixos)
