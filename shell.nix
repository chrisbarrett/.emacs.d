{ pkgs ? import <nixpkgs> { } }:

let
  emacsCustom = pkgs.emacsWithPackages
    (epkgs: (with epkgs.melpaStablePackages; [ buttercup ]));

in pkgs.mkShell { buildInputs = [ emacsCustom ]; }
