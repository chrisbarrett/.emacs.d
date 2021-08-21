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
    emacsOverlayRev = "e3179ac580e0401ee1f793f1ec30814fae13f3c7";
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
      }:
      pkgs.callPackage ./builder.nix {
        inherit emacs name buildInputs patches preBuild;
        src = pkgs.fetchFromGitHub { inherit sha256 repo rev owner; };
      };

    withPatches = pkg: patches: pkg.overrideAttrs (attrs: { inherit patches; });
  };

  builder = lispPkgs.emacsPackagesNgGen emacs;

  emacsWithPackages =
    (builder.overrideScope' packages.overrides).emacsWithPackages
      packages.packages;

  customPathEntries = strings.concatStringsSep ":" [
    "${requiredPrograms}/bin"
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
          --set NIX_EMACS_SRC_DIR "${emacs}/share/emacs/src/" \
          --set NIX_EMACS_TEX_PROGRAM "${pkgs.tectonic}/bin/tectonic" \
          --set NIX_EMACS_PLANTUML_JAR "${pkgs.plantuml}/lib/plantuml.jar" \
          --set NIX_EMACS_MU_BINARY "${pkgs.mu}/bin/mu" \
          --set NIX_EMACS_MU_LISP_DIR "${pkgs.mu}/share/emacs/site-lisp/mu4e" \
          --set JAVA_HOME "${pkgs.jdk}"
      fi
    done
  '';
}
