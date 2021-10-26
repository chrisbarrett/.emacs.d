#!/usr/bin/env bash
set -e -o pipefail

PACKAGE_ARRAY='[
  "bash-language-server",
  "dockerfile-language-server-nodejs",
  "eslint",
  "flow-bin",
  "graphql-language-service-cli",
  "typescript",
  "typescript-language-server",
  "vscode-css-languageserver-bin",
  "vscode-html-languageserver-bin",
  "vscode-json-languageserver",
  "yaml-language-server"
]'

NODE2NIX=$(nix-build '<nixpkgs>' --no-out-link -A 'nodePackages.node2nix')/bin/node2nix

# shellcheck disable=SC2039
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"
"$NODE2NIX" -i <(echo "$PACKAGE_ARRAY") --nodejs-14
