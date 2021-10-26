{ pkgs, stdenv, fetchurl }:

let
  nodePackages = pkgs.callPackage ./default.nix { };
in
nodePackages // {
  yaml-language-server = nodePackages.yaml-language-server.override
    {
      # HACK: Optional dep that's actually imported at runtime :/
      preRebuild = ''
        cd $out/lib/node_modules/yaml-language-server
        npm install prettier
      '';
    };
}
