---
title: nix-instantiate
date: 2018-12-27
info: |
  I like NixOS and the way modules work. I miss them when I do tasks in other languages that have less power than NixOS, for example Ansible and Terraform.
  Luckily all these tools can be configured via JSON, and Nix can easily create JSON. 
---

# nix-instantiate

I like NixOS and the way modules work.
I miss them when I do tasks in other languages
that have less power than NixOS, for example
[Ansible](https://www.ansible.com) and [Terraform](https://www.terraform.io).

Luckily all these tools can be configured via JSON,
and Nix can easily create JSON.
The go-to tool for that job is
[nix-instantiate](https://nixos.org/nix/manual/#sec-nix-instantiate)
which every NixOS has installed (yeye!).

## Small overview

I will show you how easy it is, with a few lines of nix,
to create a JSON renderer for terraform configuration files.
But this is only done in a sketchy way,
to inspire you to create your own setup for
different tools that use JSON.

If you are interessted in a full (or almost full)
terraform JSON renderer have a look at my
[terranix project](https://github.com/mrVanDalo/terranix).

## First tests

Lets look what `nix-instantiate` does.

We create a file `test1.nix`:

```nix
# file test1.nix
rec {
  i = "like Nix";
  you = i;
}
```

and than we run `nix-instantiate` to render JSON:

```sh
$> nix-instantiate --eval --json --strict test1.nix | jq

{
  "i": "like Nix",
  "you": "like Nix"
}
```

Nice! This is expected because it is an example from
[the documentation](https://nixos.org/nix/manual/#sec-nix-instantiate).

## Modules for the win

Modules are one of the things that make NixOS really awesome.
So lets us them in combination with `nix-instantiate`!

```nix
# file test2.nix
let
  pkgs = import <nixpkgs> {};

  result =
    with pkgs;
    with lib;
    evalModules {
      modules = [
        # option definition
        {
          options = {
            resource = mkOption {
              type    = with types; attrsOf attrs;
              default = {};
            };
          };
        }
        # config definition
        {
          resource."random_pet" = {
            "house_pet".length = 10;
            "neighbours_pet".length = 10;
          };
        }
      ];
    };
in
  result.config
```

When running:

```sh
nix-instantiate --eval --strict --json test2.nix --show-trace | jq
```

We get the following JSON:

```json
{
  "_module": {
    "args": {},
    "check": true
  },
  "resource": {
    "random_pet": {
      "house_pet": {
        "length": 10
      },
      "neighbours_pet": {
        "length": 10
      }
    }
  }
}
```

This is almost what we want to see. the `_module` value is not needed.
So let's remove it with a sanitization function, and move the content path
to a different file called `config.nix`.

```nix
# file test3.nix
let
  pkgs = import <nixpkgs> {};

  sanitize =
    with pkgs;
    configuration:
    builtins.getAttr (builtins.typeOf configuration) {
        bool = configuration;
        int = configuration;
        string = configuration;
        list = map sanitize configuration;
        set = lib.mapAttrs
          (lib.const sanitize)
          (lib.filterAttrs (name: value: name != "_module" && value != null) configuration);
      };

  result =
    with pkgs;
    with lib;
    evalModules {
      modules = [ { imports = [ ./config.nix ]; } ];
    };
in
  # whitelist the resource attribute
  { resource = (sanitize result.config).resource ; }
```

In `config.nix` we can now focus on the configuration content. And we write it
just like we would write a NixOS module.

```nix
# config.nix
{ config, lib, ... }:
with lib;
{
  options = {
    resource = mkOption {
      type    = with types; attrsOf attrs;
      default = {};
    };
  };

  config = {
    resource."random_pet" = {
      "house_pet".length = 10;
      "neighbours_pet".length = 10;
    };
  };
}
```

The result of the now well known command

```nix
nix-instantiate --eval --strict --json test3.nix --show-trace | jq
```

looks like the result we want to have:

```nix
{
  "resource": {
    "random_pet": {
      "house_pet": {
        "length": 10
      },
      "neighbours_pet": {
        "length": 10
      }
    }
  }
}
```

Now we have the full power of the NixOS module system to generate
JSON.
We can write modules to hide complexity and create very well readable
`terraform` or `ansible` setups without the need of their
strange tooling which is not capable of mapping, filtering
or hiding complexity.

## A Simple Example

Let's make an example so a non-NixOS-veteran can see
how to start using this modules system.

### `hcloud.nix`

The following file is a module that let's us create
resource entries to create an
[hcloud server](https://www.terraform.io/docs/providers/hcloud/r/server.html).
But it has one parameter `additionalFileSize`
which will automatically add an `hcloud_volume` and an `hcloud_volume_attachment`.

```nix
# hcloud.nix
{ config, lib, ... }:
with lib;
let
  cfg = config.hcloud.server;
in {
  options.hcloud.server = mkOption {
    default = {};
    type = with types; attrsOf (submodule ( { name, ... }: {
      options = {
        # mandatories : because no default is set
        server_type = mkOption {
          type    = with types; enum ["cx11" "cx21" "cx31" "cx41" "cx51"];
        };
        image = mkOption {
          type    = with types; string;
          example = "ubuntu";
          description = ''
            image to install
          '';
        };
        # optionals
        additionalFileSize = mkOption {
          type    = with types; nullOr ints.positive;
          default = null;
          description = ''
            extra volume (in GB) that should be added.
          '';
        };
      };
    }));
  };

  config =
    let
      serverResources = {
        resource.hcloud_server =
          mapAttrs (name: configuration: {
            inherit (configuration) server_type image;
            name = name;
          } ) cfg;
      };

      additionals = filterAttrs (_: configuration: configuration.additionalFileSize != null) cfg;
      additionVolumesResources = {
        resource."hcloud_volume" = mapAttrs' (name: configuration:
          nameValuePair "${name}" {
            name = "${name}_volume";
            size = configuration.additionalFileSize;
          }
        ) additionals;
      };
      additionVolumesResourcesAttatchments = {
        resource."hcloud_volume_attachment" = mapAttrs' (name: configuration:
          nameValuePair "${name}_volume_attachment" {
            volume_id = "\${hcloud_volume.${name}.id}";
            server_id = "\${hcloud_server.${name}.id}";
            automount = true;
          }
        ) additionals;
      };
    in
      mkMerge [
        serverResources
        ( mkIf ( length ( attrNames additionals ) > 0 )
          additionVolumesResources )
        ( mkIf ( length ( attrNames additionals ) > 0 )
          additionVolumesResourcesAttatchments )
      ];
}
```

### `config.nix` and Output

#### Without `additionalFileSize`

Let's look at the different `config.nix` results.

```nix
{
  imports = [
    ./core.nix       # resource definition
    ./hcloud.nix     # the hcloud_server module
  ];
  # define a hcloud_server
  hcloud.server = {
    "test" = {
      image = "ubuntu";
      server_type = "cx11";
    };
  };
}
```

```json
$> nix-instantiate --eval --strict --json test3.nix --show-trace | jq
{
  "resource": {
    "hcloud_server": {
      "test": {
        "image": "ubuntu",
        "name": "test",
        "server_type": "cx11"
      }
    }
  }
}
```

The output is like we expected it to be.

#### With `additionalFileSize`

Let's add some `additionalFileSize`.

```nix
{
  imports = [
    ./core.nix       # resource definition
    ./hcloud.nix     # the hcloud_server module
  ];
  # define a hcloud_server
  hcloud.server = {
    "test" = {
      image = "ubuntu";
      server_type = "cx11";
      additionalFileSize = 100;
    };
  };
}
```

```json
$> nix-instantiate --eval --strict --json test3.nix --show-trace | jq
{
  "resource": {
    "hcloud_server": {
      "test": {
        "image": "ubuntu",
        "name": "test",
        "server_type": "cx11"
      }
    },
    "hcloud_volume": {
      "test": {
        "name": "test_volume",
        "size": 100
      }
    },
    "hcloud_volume_attachment": {
      "test_volume_attachment": {
        "automount": true,
        "server_id": "${hcloud_server.test.id}",
        "volume_id": "${hcloud_volume.test.id}"
      }
    }
  }
}
```

Whoa, a lot of other resources joined the party.
Additionally, the `additionalFileSize` parameter is
properly removed from `resource.hcloud_server.test`.

You could also create this very simple example in `HCL` by using 
`variables`, `locals` and `count`.
By doing that, you already reached the limits of
`HCL` but in Nix this is a very simple example.

## A More Complex Example

Let's create something you wouldn't be able to do in `HCL`
anymore.

Imagine you have an inner circle of admins,
which need access to all machines created.
So when a machine is created we also add
all admin keys.

Let's look at the `config.nix` first.

```nix
{
  imports = [
    ./core.nix
    ./hcloud.nix
    ./admins.nix
  ];

  # all mighty admins
  admins = {
    palo.ssh_key = "ssh-rsa AAAAB3NzaC1yc2EAAAA......hKIWndLJ palo@someMachine";
    tv.ssh_key = "ssh-rsa AAAAB3NzaC1yc2EAAAA......hKIWndLJ tv@someMachine";
    lass.ssh_key = "ssh-rsa AAAAB3NzaC1yc2EAAAA......hKIWndLJ lass@someMachine";
  };

  hcloud.server = {
    "test" = {
      image = "ubuntu";
      server_type = "cx11";
    };
  };
}
```

We want to define the admin keys "globally" without setting them
for every machine explicitly.

### `admins.nix`

The `admins` module will not create any `resource` directly.
Instead it defines options which can be set and used by
other modules.

```nix
# admins.nix
{ lib, ... }:
with lib;
{
  options.admins = mkOption {
    default = {};
    type = with types; attrsOf ( submodule ( { name, ... }: {
      options = {
        ssh_key = mkOption {
          type    = with types; string;
          description = ''
            public key of admin.
          '';
        };
      };
    }));
  };
}
```

### hcloud.nix

The `admins` options are used in the `hcloud.nix` file, and of course every
other module that create servers.

They are accessed via `config.admins`
and depending on their content,
we create `hcloud_ssh_keys` and add them to the servers.

```nix
{ config, lib, ... }:
with lib;
let
  cfg = config.hcloud.server;
in {

  options.hcloud.server = mkOption {
    default = {};
    type = with types; attrsOf (submodule ( { name, ... }: {
      options = {
        # mandatories : because no default is set
        server_type = mkOption {
          type    = with types; enum ["cx11" "cx21" "cx31" "cx41" "cx51"];
        };
        image = mkOption {
          type    = with types; string;
          example = "ubuntu";
          description = ''
            image to install
          '';
        };
        # optionals
        additionalFileSize = mkOption {
          type    = with types; nullOr ints.positive;
          default = null;
          description = ''
            extra volume (in GB) that should be added.
          '';
        };
      };
    }));
  };

  config =
    let
      serverResources = {
        resource.hcloud_server =
          mapAttrs (name: configuration: {
            inherit (configuration) server_type image;
            name = name;

          # we add the ssh key ids, if admins exist
          } // (optionalAttrs (length adminSshKeyIds > 0) { ssh_keys = adminSshKeyIds; })

          ) cfg;
      };

      additionals = filterAttrs (_: configuration: configuration.additionalFileSize != null) cfg;
      additionVolumesResources = {
        resource."hcloud_volume" = mapAttrs' (name: configuration:
          nameValuePair "${name}" {
            name = "${name}_volume";
            size = configuration.additionalFileSize;
          }
        ) additionals;
      };
      additionVolumesResourcesAttatchments = {
        resource."hcloud_volume_attachment" = mapAttrs' (name: configuration:
          nameValuePair "${name}_volume_attachment" {
            volume_id = "\${hcloud_volume.${name}.id}";
            server_id = "\${hcloud_server.${name}.id}";
            automount = true;
          }
        ) additionals;
      };

      adminSshKeyIds = map (name: "\${hcloud_ssh_key.${name}.id}") (attrNames config.admins);
      adminSshKeys = {
        resource."hcloud_ssh_key" = mapAttrs (name: configuration: {
          name = name;
          public_key = configuration.ssh_key;
        }) config.admins; };
    in
      mkMerge [
        serverResources
        ( mkIf ( length ( attrNames additionals ) > 0 )
          additionVolumesResources )
        ( mkIf ( length ( attrNames additionals ) > 0 )
          additionVolumesResourcesAttatchments )

        # we create hcloud_ssh_keys, if admins exist
        ( mkIf ( length ( attrNames config.admins ) > 0 )
          adminSshKeys)

      ];
}
```

The `hcloud.nix` starts to get big now, but it is very similar to the version
from the privious section.
Focus on the last `let` section and on `mkMerge`.
Look closely at the end of the `serverResource` definition.

### Output

Let's look at the resulting JSON:

```json
$> nix-instantiate --eval --strict --json test3.nix --show-trace | jq
{
  "resource": {
    "hcloud_server": {
      "test": {
        "image": "ubuntu",
        "name": "test",
        "server_type": "cx11",
        "ssh_keys": [
          "${hcloud_ssh_key.lass.id}",
          "${hcloud_ssh_key.palo.id}",
          "${hcloud_ssh_key.tv.id}"
        ]
      }
    },
    "hcloud_ssh_key": {
      "lass": {
        "name": "lass",
        "public_key": "ssh-rsa AAAAB3NzaC1yc2EAAAA......hKIWndLJ lass@someMachine"
      },
      "palo": {
        "name": "palo",
        "public_key": "ssh-rsa AAAAB3NzaC1yc2EAAAA......hKIWndLJ palo@someMachine"
      },
      "tv": {
        "name": "tv",
        "public_key": "ssh-rsa AAAAB3NzaC1yc2EAAAA......hKIWndLJ tv@someMachine"
      }
    }
  }
}
```

Nice! All 3 keys will be created by `hcloud_ssh_key` and they all get wired
to the new `hcloud_server`.

This should give you a feeling how you can maintain your
JSON/YAML-configured tools, with `nix-instantiate` and the NixOS module system.

Happy Hacking!

## Thanks

Thanks to `tv` for his introduction to `nix-instantiate`.

Thanks to `lassulus` and `kmein` for polishing this article.
