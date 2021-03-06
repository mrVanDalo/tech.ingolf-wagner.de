---
title: terranix
date: 2019-08-23
info: An introduction on terranix for nixos users.
scheme: light
qrcode-link: qr-tech-ingolf-wag.svg
---

# What is Terraform

* A declarative configuration tool to describe state behind an API
* AWS, Azure, Hetzner, Google Cloud, Akamai, Github, Grafana, [and more](https://www.terraform.io/docs/providers/index.html)
* comes with a painful language called [HCL](https://www.terraform.io/docs/configuration/syntax.html).

---

# What is terranix?

* Tool to render nix-modules to terraform json
* Is much more pleasant and powerful than [HCL](https://www.terraform.io/docs/configuration/syntax.html) 

---

# Workflow

```
config.nix → config.tf.json → terraform.state
```

```shell
terranix > config.tf.json && terraform init && terraform apply
```
---

## config.nix example (provider)

A small example to define some repositories and users in [github](https://www.terraform.io/docs/providers/github/index.html).

```nix
# Configure the GitHub Provider
provider.github = {
  token = "\${var.github_token}";
  organization = "nixos-community";
};
```
---

## config.nix example (code)
```nix
# Create a github repository
resource.github_repository.example = {
  name = "example";
  description = "My awesome codebase";
};

# Add a collaborator to a repository
resource.github_repository_collaborator.a_repo_collaborator = {
  repository = config.resource.github_repository.example.name;
  username = config.data.github_user.palo.username;
  permission = "push";
};

# query an existing user
data.github_user.palo.username = "mrVanDalo";
```

---

# Merging configuration

* merging `resources`, `data`, `output`, ... is not possible.
* you have to create an opinionated module with proper types to get them merged.

--
 
```nix
# server.nix
resource.hcloud_server.example ={
  name = "example";
  image = "debian-9";
};

# config-testing.nix
resource.hcloud_server.example.server_type = "cx11";

# config-production.nix
resource.hcloud_server.example.server_type = "cx31";
```
---

# a basic setup

* an example setup is at [nix-shell-mix](https://github.com/mrVanDalo/nix-shell-mix/blob/master/terraform/shell.nix)

### shell.nix

```nix
pkgs.writeShellScriptBin "terraform" let
  pass = id: "${pkgs.pass}/bin/pass ${id}"
in ''
  export TF_VAR_github_token=`${pass github/token}`
  ${pkgs.terraform}/bin/terraform "$@"
'';
```

---

# Road  map

* documentation generation like for `man configuration.nix` in nixos for your own modules
* resource merging solution that is easy to hack, and adept to even with a large code base.
* always be backwards compatible.
* Better documentation.

