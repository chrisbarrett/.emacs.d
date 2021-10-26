#!/usr/bin/env bash
set -e -o pipefail

# shellcheck disable=SC2039
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"

NODE2NIX=$(nix-build '<nixpkgs>' --no-out-link -A 'nodePackages.node2nix')/bin/node2nix
"$NODE2NIX" -i node-packages.json --nodejs-14
