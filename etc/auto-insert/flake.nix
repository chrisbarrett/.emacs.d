# name: flake.nix
# match: (rx "flake.nix" string-end)
# --
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        name = "$0";
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ ];
        };
      });
}