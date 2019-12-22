---
title: krops
info: |
  krops is an alternative to
  NixOps trying to solve some of theses flaws,
  with some very simple concepts.
  In this article we will checkout on how to deploy a server
  using krops and discuss all involved parameters and steps.
date: 2018-08-15
---

# krops

[NixOps](https://nixos.org/nixops/)
the official DevOps tool of
[NixOS ](https://nixos.org) is nice, but it has some flaws.
[krops](https://cgit.krebsco.de/krops/about/) is an alternative to
[NixOps](https://nixos.org/nixops/) trying to solve some of theses flaws,
with some very simple concepts.

If you're looking for a good document on how to use
[NixOps](https://nixos.org/nixops/) in the fields,
have a look at
[this excellent article](https://blog.wearewizards.io/how-to-use-nixops-in-a-team).

# krops vs. NixOps (Feature Comparison)

<table class="comparison">
  <thead>
    <tr>
      <th class="text">Feature</th>
      <th>NixOps</th>
      <th>krops</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td title="serverA is unstable, serverB is stable, serverC on commit=XY">
        Precise versioning for every machine.
      </td>
      <td class="bad"> No </td>
      <td class="good"> Yes </td>
    </tr>
    <tr>
      <td class="text">Well documented</td>
      <td class="good"> Yes </td>
      <td class="bad"> No</td>
    </tr>
    <tr>
      <td class="text">Lightweight</td>
      <td class="ok"> Kinda </td>
      <td class="good"> Yes </td>
    </tr>
    <tr>
      <td class="text"
          title="krops uses passwordstore.org, see more below">
        Native File Encryption
      </td>
      <td class="bad"> No </td>
      <td class="good"> Yes </td>
    </tr>
    <tr>
      <td class="text"
          title="nixops has deployment.keys">
        TMPFS Key Management
      </td>
      <td class="good"> Yes </td>
      <td class="bad"> No </td>
    </tr>
    <tr>
      <td class="text"
          title="run nixos-rebuild on the target system">
        Manual Deployment Possible
      </td>
      <td class="bad"> No </td>
      <td class="good"> Yes </td>
    </tr>
    <tr>
      <td class="text" >
        Needs Database
      </td>
      <td class="bad"> Yes </td>
      <td class="good"> No </td>
    </tr>
    <tr>
      <td class="text"
          title="where are the .drv files are created">
        Build and Download happens on
      </td>
      <td class="ok"> Client </td>
      <td class="good"> Target </td>
    </tr>
  </tbody>
</table>

# krops Structure by Example

krops is not an executable like NixOps,
it is a library you use to write executables which do the actual deployment.

Let's say you have a very simple `configuration.nix`

```nix
{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.git ];
}
```

Than you can use the following script (let's name it `krops.nix`) to deploy it
on the machine `server01.mydomain.org`.

```nix
let
  krops = builtins.fetchGit {
    url = "https://cgit.krebsco.de/krops/";
  };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  source =  lib.evalSource [
    {
      nixpkgs.git = {
        ref = "nixos-19.03";
        url = https://github.com/NixOS/nixpkgs-channels;
      };
      nixos-config.file = toString ./configuration.nix;
    }
  ];

in {
  server01 = pkgs.krops.writeDeploy "deploy-server01" {
    source = source;
    target = "root@server01.mydomain.org";
  };  
}
```

Now you can deploy the machine by running:

```shell
$> nix-build ./krops.nix -A server01 && ./result
```

You need to make sure you have ssh access to the root user on `server01.mydomain.org`
and `git` is installed on `server01.mydomain.org`.

<div class="panel panel-info">

If you run this command the first time you will most likely get a message like

```output
error: missing sentinel file: server01.mydomain.org:/var/src/.populate
```

This is because you need to create `/var/src/.populate` before krops will do anything.
Once that file is created, you can run the command `./result` again.

</div>

krops will copy the file `configuration.nix` to `/var/src/nixos-config` on `server01`
and will clone `nixpkgs` into `/var/src/nixpkgs`.
After that, krops will run `nixos-rebuild switch -I /var/src` which will provision
`server01`.

## The Different Parts Explained

Let's start with the cryptic part at the beginning.

```nix
let
  krops = builtins.fetchGit {
    url = "https://cgit.krebsco.de/krops/";
  };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};
```

It downloads krops and makes its library and packages available
so they can be used it in the following script.

```nix
in {
  server01 = pkgs.krops.writeDeploy "deploy-server01" {
    source = source;
    target = "root@server01.mydomain.org";
  };
}
```

The executable `server01` is which results in the link `./result`.
It is the result of `krops.writeDeploy` with parameters

* `target` passed to the ssh command
* `source` the set of files and folders which should be made available beneath
  `/var/src` on the target

<div class="panel panel-info">

`target` takes more argument parts than just the host,
you can for example set it to `root@server01:4444/etc/krops/`

to change the ssh port and the target folder it should be copied.

</div>



```nix
  source =  lib.evalSource [
    {
      nixpkgs.git = {
        ref = "nixos-18.03";
        url = https://github.com/NixOS/nixpkgs-channels;
      };
      nixos-config.file = toString ./configuration.nix;
    }
  ];
```

The list of folders and files are managed by the `source` parameter.
The keys in will be the names of the folders or files in `/var/src`.
`nixpkgs` and `nixos-config` are mandatory.

All other files/folders must be referenced in the resulting `nixos-config` file.

## Different Sources

### Files and Folders

You can use the `file` attribute to transfer
files and folders from the build host to the target host.
But it always must be an absolute path.

```nix
source =  lib.evalSource [
  {
    modules.file = toString ./modules; # toString generates an absoulte path
  }
];
```

This copies `./modules` to `/var/src/modules`.

### Symlinks

You can also use the `symlink` argument
to create symlinks on the target system.

```nix
source =  lib.evalSource [
  {
    config.file        = toString ./config;
    nix-config.symlink = "config/server01/configuration.nix";
  }
];
```

This copies `./config` to `/var/src/config` and creates a symlink
`/var/src/nix-config` to `config/server01/configuration.nix`.

<div class="panel panel-warning">

krops will not check if the target is valid.

</div>


### Git Repositories

You can pull Git repositories using the `git` attribute
from everywhere you want,
as long as the target host is able to pull it.

```nix
source =  lib.evalSource [
  {
    nix-writers.git = {
      url = https://cgit.krebsco.de/nix-writers/;
      ref = "4d0829328e885a6d7163b513998a975e60dd0a72";
    };
  }
];
```

This pulls the [nix-writers](https://cgit.krebsco.de/nix-writers/)
repository
to `/var/src/nix-writers`.

the `ref` parameter also accepts branches or tags.

### Password Store (Native File Encryption)

lets assume `secrets` is a folder managed by
[passwordstore](https://www.passwordstore.org/).

```nix
secrets
|-- server01
|   `-- wpa_supplicant.conf.gpg
`-- server02
    `-- wpa_supplicant.conf.gpg
```

Use the `pass` argument to include the sub-folder `server01`
into your deployment.

```nix
source = lib.evalSource [
  {
    secrets.pass = {
      dir  = toString ./secrets;
      name = "server01";
    };
  }
];

```

This copies `secrets/server01` to `/var/src/secrets` after it is decrypted.
You will be prompted to enter the password.

<div class="panel panel-warning">
The files in `/var/src/secrets` will be unencrypted!
</div>


## How to use Sources in configuration.nix

You can use folders copied by krops
very pleasantly in the `configuration.nix`.

```nix
{ config, libs, pkgs, ... }:
{
  imports = [
    <modules>
    <config/service01/hardware-configuration.nix>
  ];
  networking.supplicant."wlan0".configFile.path = toString <secrets/wpa_supplicant.conf>;
}
```

## How to Manually Rebuild the System

If you, for some reason, want to rebuild the system on the host itself,
you can do that simply by running as root

```shell
#> nixos-rebuild switch -I /var/src
```

# Some Tips

So far this is everything krops does.
It is simple and very close to the usual way Nix and NixOS works.
Let's look on some common pattern to solve some common issues.

## Multiple Server

If you want to manage multiple computers,
the following adjustments might help you.

Take a closer look to the `source` function and the parameter
`nixos-config` and `secrets`.

```nix
let
  source = name: lib.evalSource [
    {
      config.file  = toString ./config;
      modules.file = toString ./modules;
      nixos-config.symlink = "config/${name}/configuration.nix"
      nixpkgs.git = {
        ref = "nixos-18.03";
        url = https://github.com/NixOS/nixpkgs-channels;
      };
      secrets.pass = {
        dir  = toString ./secrets";
        name = "${name}";
      };
    }
  ];

  server01 = pkgs.krops.writeDeploy "deploy-server01" {
    source = source "server01";
    target = "root@server01.mydomain.org";
  };

  server02 = pkgs.krops.writeDeploy "deploy-server02" {
    source = source "server02";
    target = "root@server02.mydomain.org";
  };

in {
  server01 = server01;
  server02 = server02;
  all = pkgs.writeScript "deploy-all-servers"
    (lib.concatStringsSep "\n" [ server01 server02 ]);
}

```

Now you can create multiple `./result`s or you can use the
`-A` parameter of nix-build to choose what `./result` will be.

```shell
$> nix-build ./krops.nix -A server01 && ./result
$> nix-build ./krops.nix -A server02 && ./result
$> nix-build ./krops.nix -A all && ./result
```

## Update and Fixing Git Commits

Updating hashes for Git repositories is annoying and using branches
might break consistency.
To avoid editing files you can use the `nix-prefetch-git`
and `lib.importJson` to make your live easier.

```shell
$> nix-prefetch-git \
  --url https://github.com/NixOS/nixpkgs-channels \
  --rev refs/heads/nixos-18.03 \
  > nixpkgs.json
```

results in a file `nixpkgs.json` which looks like this

```json
{
  "url": "https://github.com/NixOS/nixpkgs-channels.git",
  "rev": "9cbc7363543ebeb5a0182aa171f23bb19332b99f",
  "date": "2018-08-14T14:00:50+02:00",
  "sha256": "1i3iwc23cl085w429zm6qip1058bsi7zavj7pdwqiqm9nymy7plq",
  "fetchSubmodules": true
}
```

And it can be imported in `./krops.nix` like this.

```nix
let
  importJson = (import <nixpkgs> {}).lib.importJSON;
  source = lib.evalSource [
    {
      nixpkgs.git = {
        ref = (importJson ./nixpkgs.json).rev;
        url = https://github.com/NixOS/nixpkgs-channels;
      };
    }
  ];
```

Now you can just have to call the `nix-prefetch-git` command
and the commit reference will be updated, and is fixed.

This should also make it simpler to maintain different channels on different machines.

## Use Packages from other channels

It is very easy to install packages from different channels.

For example add `nixpkgs-unstable` the same way you add `nixpkgs`.

```nix
  source =  lib.evalSource [
    {
      nixpkgs.git = {
        ref = "nixos-18.09";
        url = https://github.com/NixOS/nixpkgs-channels;
      };
      nixpkgs-unstable.git = {
        ref = "nixos-unstable";
        url = https://github.com/NixOS/nixpkgs-channels;
      };
      nixos-config.file = toString ./configuration.nix;
    }
  ];
```

To install a package from the `unstable` channel you just have to import the channel
and call the packages from there.

```nix
{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> {};
in {
  environment.systemPackages = [

    # install gimp from stable channel
    pkgs.gimp

    # install inkscape from unstable channel
    unstable.inkscape

  ];
}
```

## Channels and NIX_PATH

You might wonder how `nix-shell` is catching up with the
`nixpkgs` in `/var/src`.

`nix-shell` will still use the standard system setup,
including your channel configurations,
which you have to maintain on top of using krops.

If you don't like to do that (like me) you have to change
the `NIX_PATH` variable system-wide.

```nix
environment.variables.NIX_PATH = lib.mkForce "/var/src";
```

And `nix-shell` will also use `nixpkgs` from `/var/src`
