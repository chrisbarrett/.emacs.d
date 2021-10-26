{ pkgs ? import <nixpkgs> { }, lib ? pkgs.lib }:

let
  node-language-servers = lib.attrsets.attrValues
    (lib.attrsets.filterAttrs
      (_name: lib.attrsets.isDerivation)
      (pkgs.callPackage ./override.nix { }));
in
pkgs.mkShell {
  buildInputs = node-language-servers;
}
