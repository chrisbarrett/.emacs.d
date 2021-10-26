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
    emacsOverlayRev = "3fbe6cf3459cf955e188444b68f085f7a45b6ffa";
  }
  # Version of nixpkgs that determines 3rd-party Lisp package versions.
, lispPkgs ? nixpkgsWithOverlays {
    emacsOverlayRev = "dcdb2e20a30c851b204cd0ab7a7d4745c6af7ff6";
  }
}:

let
  inherit (pkgs.lib) strings;

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
    ];
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

  languageServers = pkgs.callPackage ./language-servers { };

  customPathEntries = strings.concatStringsSep ":" [
    "${requiredPrograms}/bin"
    "${languageServers}/bin"
    "${pkgs.jdk}/bin"
  ];

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
          --set NIX_EMACS_DARWIN_PATH_EXTRAS "${customPathEntries}" \
          --set NIX_EMACS_ESLINT_SERVER_SCRIPT "${languageServers}/lib/eslintServer.js" \
          --set NIX_EMACS_GROOVY_LANGUAGE_SERVER_JAR "${languageServers}/lib/groovy-ls/groovy-ls.jar" \
          --set NIX_EMACS_LSP_ESLINT_NODE_PATH "${pkgs.nodejs}/bin/node" \
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
