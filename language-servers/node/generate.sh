#!/usr/bin/env bash

PACKAGE_ARRAY='[
  "bash-language-server",
  "dockerfile-language-server-nodejs",
  "eslint",
  "flow-bin",
  "typescript",
  "typescript-language-server",
  "vscode-css-languageserver-bin",
  "vscode-html-languageserver-bin",
  "vscode-json-languageserver"
]'

NODE2NIX=$(nix-build '<nixpkgs>' --no-out-link -A 'nodePackages.node2nix')/bin/node2nix

"$NODE2NIX" -i <(echo "$PACKAGE_ARRAY") --nodejs-12
