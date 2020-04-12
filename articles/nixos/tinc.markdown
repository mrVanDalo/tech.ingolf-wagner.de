---
title: tinc
date: 2018-07-07
info: |
  In this article I will describe how to use my
  NixOS tinc-module.
  I had to write my own,
  because the standard `services.tinc` module
  misses a lot of features,
  for example forwarding to sub-networks and network wise activation and deactivation.
  I designed it to work nicely with
  NixOps.
---

# Tinc

In this article I will describe how to use my
[NixOS](https://nixos.org/)
tinc-module.
I had to write my own,
because the standard `services.tinc` module
misses a lot of features,
for example forwarding to sub-networks and network wise activation and deactivation.
I designed it to work nicely with
[NixOps](https://nixos.org/nixops/).

You have to `enable` and `disable ` every network you define,
instead of `enable ` tinc which enables all defined networks.
This should make it easy to define all your networks
in one file (to keep track about everything),
and micromanage in the computer specific definitions.

# How to import

You can use `fetchgit` to import it without downloading it yourself.

<div class="panel panel-info">
To find the newest `rev` and `sha256` just call `nix-shell -p nix-prefetch-git --run "nix-prefetch-git https://github.com/mrVanDalo/nixos-tinc.git"`
</div>

```
{pkgs, ... }:
let
  tincModule = pkgs.fetchgit {                                                     
    "url" = "https://github.com/mrVanDalo/nixos-tinc.git";             
    "rev" = "1b8c822a20be38ca19120f4c5d7f9f3236851674";              
    "sha256" = "0a99rxhrqwh454q6w1znb0icdpm8fl4rnr1dr7xblacqjmghhyh1";
  };

in {
  imports = [
    "${tincModule}"
  ];
}
```

Now you are ready to go!

# Create Keys

First of all,
we have to create tinc keys for every computer in the VPN Mesh.
To put the keys in the current folder we use the `'--config .'` option,

```
$> nix-shell -p tinc_pre --run "tinc --config . generate-keys 4096"
```

After that is done we create the `hostfile` by

```
$> cat *.pub > hostfile ; rm *.pub
```

## Key-File-structure for these Examples

In the following examples I expect the following file-structure for keys and config files.

```
|-- public
|   |-- Gibson
|   |   `-- hostfile
|   |-- Hackbardt
|   |   `-- hostfile
|   `-- HAL
|       `-- hostfile
`-- secrets
    |-- Gibson
    |   |-- ed25519_key.priv
    |   `-- rsa_key.priv
    |-- Hackbardt
    |   |-- ed25519_key.priv
    |   `-- rsa_key.priv
    `-- HAL
        |-- ed25519_key.priv
        `-- rsa_key.priv
```


# Connect 3 computers

We want to connect 3 computers in a private network of range ``10.1.1.0/24``.
One computer needs to be accessable from the internet,
it will be the computer that connects all the other computer.
You can have multiple computers which are reachable from the internet
but for this example we only have one.


![](/images/3computers.svg)


Here is the `configuration.nix`.
First we define the whole topolgy in `default` and
than we `enable` and configure secret-keys
for every computer in `Gibson`, `Hackbardt` and `HAL`

```
let

  includePrivateKeys = host: {
    deployment.keys."rsa_key" = {
      keyFile = ./secrets/"${host}"/rsa_key.priv;
      destDir = "/root/secrets";
    };
    deployment.keys."ed25519_key" = {
      keyFile = ./secrets/"${host}"/ed25519_key.priv;
      destDir = "/root/secrets";
    };
  };


in {

# for all machines
# ----------------
default =
{config, pkgs, lib, ... }:
{
  with lib;
  services.custom.tinc =
  let
    publicHostFile = host: fileContent ./public/"${host}"/hostfile;
  in {
    "private" = {
      debugLevel    = 0;
      port          = 655;
      networkSubnet = "10.1.1.0/24";
      hosts = {
        Gibson = {
          realAddress = [ "my.awesome.dns.com" ];
          tincIp      = "10.1.1.1";
          publicKey   = publicHostFile "Gibson";
        };
        Hackbardt = {
          tincIp      = "10.1.1.2";
          publicKey   = publicHostFile "Hackbardt";
        };
        HAL = {
          tincIp      = "10.1.1.3";
          publicKey   = publicHostFile "HAL";
        };
      };
    };
  };
}

# Hackbardt specific
# ------------------
Gibson =
      enable                = true;
      connectTo             = [ "Gibson" ];
      privateRsaKeyFile     = config.deployment.keys."rsa_key".path;
      privateEd25519KeyFile = config.deployment.keys."ed25519_key".path;
    };
  };
}

# HAL specific
# ------------
HAL =
{config, pkgs, ... }:
includePrivateKeys "HAL" // {
  services.custom.tinc = {
    "private" = {
      enable                = true;
      connectTo             = [ "Gibson" ];
      privateRsaKeyFile     = config.deployment.keys."rsa_key".path;
      privateEd25519KeyFile = config.deployment.keys."ed25519_key".path;
    };
  };
}

}
```

If we deploy that and check the servers,
we can see that tinc creates interfaces called `tinc.private`.
Observing the routes we see that tinc sets up everything which is needed for proper routing.

```
$Gibson> ip addr show dev tinc.private
4: tinc.private: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 3f:ac:bd:c2:f6:9c brd ff:ff:ff:ff:ff:ff
    inet 10.1.1.1/32 scope global tinc.private
       valid_lft forever preferred_lft forever
    inet 169.254.22.60/16 brd 169.254.255.255 scope global tinc.private
       valid_lft forever preferred_lft forever
    inet6 ffa1::2afc:b2ff:fcf2:f97a/64 scope link
       valid_lft forever preferred_lft forever
```
```
$Gibson> ip route show dev tinc.private
10.1.1.0/24 scope link
169.254.0.0/16 proto kernel scope link src 169.254.22.60 metric 204
```

It also creates `/etc/host` entries `<computername>.<networkname>`
so you don't have to remember the IPs.

```
$Gibson> ping HAL.private  -c 1
PING HAL.private (10.1.1.3) 56(84) bytes of data.
64 bytes from HAL.private (10.1.1.1): icmp_seq=1 ttl=64 time=5.27 ms
```

**Awesome!** That was easy!


# Connect 2 sub-networks

So far so good,
but lets imagine we have some virtual machines running on 2 computers and want to make these virtual machines see each other.
This is a very common problem in [Kubernetes](https://kubernetes.io/).
It can be resolved by using the `tincSubnet` parameter,
to configure sub-network routing.


![](/images/2subnets.svg)


Achieving this is very simple,
just add the `tincSubnet` parameter in the `hosts` attribute and your done.

```
...

default =
{config, pkgs, lib, ... }:
{
  with lib;
  services.custom.tinc =
  let
    publicHostFile = name: fileContent ./public/"${name}"/hostfile;
  in {
    "private" = {
      debugLevel    = 0;
      port          = 655;
      networkSubnet = "10.1.1.0/24";
      hosts = {
        Gibson = {
          realAddress = [ "my.awesome.dns.com" ];
          tincIp      = "10.1.1.1";
          publicKey   = publicHostFile "Gibson";
        };
        Hackbardt = {
          tincIp      = "10.1.1.2";
          subnetIp    = "10.2.2.0/24";
          publicKey   = publicHostFile "Hackbardt";
        };
        HAL = {
          tincIp      = "10.1.1.3";
          subnetIp    = "10.2.3.0/24";
          publicKey   = publicHostFile "HAL";
        };
      };
    };
  };
}

...
```

After deployment we can see that `Gibson` has proper routing to the configured `tincSubnet`
ranges as well as to `10.1.1.0/24` to reach the other network-nodes.

```
$Gibson> ip route show dev tinc.private
10.1.1.0/24 scope link
10.2.2.0/24 scope link
10.2.3.0/24 scope link
169.254.0.0/16 proto kernel scope link src 169.254.116.112 metric 203
```

`Hackbardt` has routing to the network provided by `HAL`,
but has no routing (on the `tinc.private` interface) to the network it provides it self.

```
$Hackbardt> ip route show dev tinc.private
10.1.1.0/24 scope link
10.2.2.0/24 scope link
169.254.0.0/16 proto kernel scope link src 169.254.116.112 metric 203
```

The module also sets the `sysctl` parameter
`net.ipv4.config.tinc/private.forwarding`
and
`net.ipv6.config.tinc/private.forwarding`
to
make sure the `tinc.private` interface forwards the traffic
to the configured sub-networks.
