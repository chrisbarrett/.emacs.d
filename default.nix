let
  nixpkgsWithOverlays = { emacsOverlayRev }:
    import <nixpkgs> {
      overlays = [
        (import ./overlays)
        # https://github.com/nix-community/emacs-overlay
        (import (builtins.fetchTarball {
          url =
            "https://github.com/nix-community/emacs-overlay/archive/${emacsOverlayRev}.tar.gz";
        }))
      ];
    };
in
{
  # Version of nixpkgs used for building binaries and Emacs itself.
  pkgs ? nixpkgsWithOverlays {
    emacsOverlayRev = "f6da7e363211fb0247785651489e5825ee8f29c6";
  }
  # Version of nixpkgs that determines 3rd-party Lisp package versions.
, lispPkgs ? nixpkgsWithOverlays {
    emacsOverlayRev = "f6da7e363211fb0247785651489e5825ee8f29c6";
  }
}:

let
  inherit (pkgs.lib) strings attrsets;

  emacs = pkgs.emacsGcc.overrideAttrs (old: {
    patches = old.patches ++ [
      ./patches/emacs/0001-optional-org-gnus.patch
      ./patches/emacs/0002-dont-warn-on-archives.patch
    ];

    postInstall = ''
      ${old.postInstall}
      cp -r $src/src $out/share/emacs/src
    '';
  });

  # Additional programs to be injected into Emacs' environment.

  requiredPrograms = pkgs.symlinkJoin {
    name = "emacs-required-programs";
    paths = with pkgs; [
      (aspellWithDicts (ps: [ ps.en ]))
      multimarkdown
      nixpkgs-fmt
      ripgrep
      sqlite
      jdk
    ];
  };

  languageServers = with pkgs; {
    bash = nodePackages.bash-language-server;
    css = nodePackages.vscode-css-languageserver-bin;
    eslint = vscode-extensions.dbaeumer.vscode-eslint;
    graphql = vscode-extensions.graphql.vscode-graphql;
    groovy = pkgs.callPackage ./language-servers/groovy-ls.nix { };
    html = nodePackages.vscode-html-languageserver-bin;
    json = nodePackages.vscode-json-languageserver;
    nix = rnix-lsp;
    openapi = pkgs.callPackage ./language-servers/aml-ls.nix { };
    terraform = terraform-lsp;
    typescript = nodePackages.typescript-language-server;
    yaml = nodePackages.yaml-language-server;
  };

  packages = pkgs.callPackage ./packages.nix rec {
    emacsmirror = args: github (args // { owner = "emacsmirror"; });

    github =
      { name
      , repo ? name
      , rev
      , owner
      , sha256
      , buildInputs ? [ ]
      , patches ? [ ]
      , preBuild ? ""
      , postInstall ? ""
      }:
      pkgs.callPackage ./builder.nix {
        inherit emacs name buildInputs patches preBuild postInstall;
        src = pkgs.fetchFromGitHub { inherit sha256 repo rev owner; };
      };

    withPatches = pkg: patches: pkg.overrideAttrs (attrs: { inherit patches; });
  };

  builder = lispPkgs.emacsPackagesNgGen emacs;

  emacsWithPackages =
    (builder.overrideScope' packages.overrides).emacsWithPackages
      packages.packages;

  customPathEntries =
    let paths = [ "${requiredPrograms}/bin" ] ++ (map (pkg: "${pkg}/bin") (attrsets.attrValues languageServers));
    in
    strings.concatStringsSep ":" paths;

in
pkgs.symlinkJoin {
  name = "emacs-wrapped";
  buildInputs = [ pkgs.makeWrapper ];
  paths = [ emacsWithPackages ];
  postBuild = ''
    for program in "$out/Applications/Emacs.app/Contents/MacOS/Emacs" "$out/bin/emacs"; do
      if [ -f "$program" ]; then
        wrapProgram "$program" \
          --prefix PATH ":" "${customPathEntries}" \
          --set NIX_EMACS_AML_LANGUAGE_SERVER_JAR "${languageServers.openapi}/lib/aml-ls/als-server.jar" \
          --set NIX_EMACS_DARWIN_PATH_EXTRAS "${customPathEntries}" \
          --set NIX_EMACS_ESLINT_SERVER_SCRIPT "${languageServers.eslint}/lib/eslintServer.js" \
          --set NIX_EMACS_GROOVY_LANGUAGE_SERVER_JAR "${languageServers.groovy}/lib/groovy-ls/groovy-ls.jar" \
          --set NIX_EMACS_LSP_ESLINT_NODE_PATH "${pkgs.nodejs}/bin/node" \
          --set NIX_EMACS_TS_LANGUAGE_SERVER "${languageServers.typescript}/bin/typescript-language-server" \
          --set NIX_EMACS_MU_BINARY "${pkgs.mu}/bin/mu" \
          --set NIX_EMACS_MU_LISP_DIR "${pkgs.mu}/share/emacs/site-lisp/mu4e" \
          --set NIX_EMACS_PLANTUML_JAR "${pkgs.plantuml}/lib/plantuml.jar" \
          --set NIX_EMACS_SRC_DIR "${emacs}/share/emacs/src/" \
          --set NIX_EMACS_TEX_PROGRAM "${pkgs.tectonic}/bin/tectonic" \
          --set JAVA_HOME "${pkgs.jdk}"
      fi
    done
  '';
}
