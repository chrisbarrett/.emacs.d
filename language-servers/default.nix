{ pkgs, lib, ... }:
let
  node-language-servers = lib.attrsets.attrValues
    (lib.attrsets.filterAttrs
      (_name: lib.attrsets.isDerivation)
      (pkgs.callPackage ./node/override.nix { }));

  aml-ls = pkgs.callPackage ./aml-ls.nix { };
  groovy-ls = pkgs.callPackage ./groovy-ls.nix { };
  terraform-ls = pkgs.callPackage ./terraform-ls.nix { };
  vscode-eslint = pkgs.callPackage ./vscode-eslint.nix { };
in
pkgs.symlinkJoin {
  name = "language-servers";
  paths = node-language-servers ++ [
    aml-ls
    groovy-ls
    vscode-eslint
  ];
  # Anything installed to /bin has to be copied, or it will be clobbered
  # by the node pkgs /bin, which is a symlink.
  postBuild = ''
    cp ${terraform-ls}/bin/* $out/bin
  '';
}
