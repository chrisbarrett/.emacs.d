{ pkgs, ... }:
let
  node = pkgs.callPackage ./node { };
  terraform-ls = pkgs.callPackage ./terraform-ls.nix { };
  vscode-eslint = pkgs.callPackage ./vscode-eslint.nix { };

  emmyLuaJar = pkgs.fetchurl {
    name = "emmy-lua.jar";
    url =
      "https://ci.appveyor.com/api/buildjobs/sq7l4h55stcyt4hy/artifacts/EmmyLua-LS%2Fbuild%2Flibs%2FEmmyLua-LS-all.jar";
    sha256 = "0pxnbrfb6n3y6a82c41f2ldnpb2r0b18z5d6c0azril5zfwjrk6l";
  };

in pkgs.symlinkJoin {
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
  ];
  postBuild = ''
    cp ${terraform-ls}/bin/* $out/bin
    mkdir -p $out/lib
    cp ${emmyLuaJar} $out/lib/emmy-lua.jar
    cp ${
      ./groovy-language-server-all.jar
    } $out/lib/groovy-language-server-all.jar
    cp ${vscode-eslint}/lib/eslintServer.js $out/lib/eslintServer.js
  '';
}
