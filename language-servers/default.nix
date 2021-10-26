{ pkgs, ... }:
let
  node = pkgs.callPackage ./node { };
  groovy-ls = pkgs.callPackage ./groovy-ls.nix { };
  terraform-ls = pkgs.callPackage ./terraform-ls.nix { };
  vscode-eslint = pkgs.callPackage ./vscode-eslint.nix { };
in
pkgs.symlinkJoin {
  name = "language-servers";
  paths = [
    node.bash-language-server
    node.dockerfile-language-server-nodejs
    node.eslint
    node.graphql-language-service-cli
    node.typescript
    node.typescript-language-server
    node.vscode-css-languageserver-bin
    node.vscode-html-languageserver-bin
    node.vscode-json-languageserver
    groovy-ls
    vscode-eslint
  ];
  # Anything installed to /bin has to be copied, or it will be clobbered
  # by the node pkgs /bin, which is a symlink.
  postBuild = ''
    cp ${terraform-ls}/bin/* $out/bin
  '';
}
