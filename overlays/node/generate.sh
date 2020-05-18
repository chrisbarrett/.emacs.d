#!/usr/bin/env bash

PACKAGE_ARRAY='[
  "prettier"
]'

NODE2NIX=$(nix-build '<nixpkgs>' --no-out-link -A 'nodePackages.node2nix')/bin/node2nix

"$NODE2NIX" -i <(echo "$PACKAGE_ARRAY") # --nodejs-10
