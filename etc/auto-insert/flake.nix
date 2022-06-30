# name: flake.nix
# match: (rx "flake.nix" string-end)
# --
{
  # description = "";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        name = "$0";
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ ];
        };
      });
}