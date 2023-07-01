# name: shell.nix
# match: (rx "shell.nix" string-end)
# --
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    $0
  ];
}
