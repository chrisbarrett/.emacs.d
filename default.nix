let
  # 2020-01-03
  overlayRev = "82a9f26161e10691233cf757bddfd9d7c70ac940";
  emacs-overlay = import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/${overlayRev}.tar.gz";
  });
in { pkgs ?
  import <nixpkgs> { overlays = [ emacs-overlay (import ./overlays) ]; } }:

let
  inherit (pkgs.lib) strings;

  emacs = pkgs.emacsGcc.overrideAttrs (old: {
    patches = old.patches ++ [
      ./patches/emacs/0001-optional-org-gnus.patch
      ./patches/emacs/0002-dont-warn-on-archives.patch
      ./patches/emacs/0003-prettier-ibuffer.patch
    ];

    postPatch = ''
      ${old.postPatch}
      # Delete the built-in orgmode.
      rm -r test/lisp/org lisp/org etc/org etc/ORG-NEWS doc/misc/org.texi
    '';

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
      nixfmt
      ripgrep
      sqlite
    ];
  };

  packages = pkgs.callPackage ./packages.nix rec {

    emacsmirror = args: github (args // { owner = "emacsmirror"; });

    github = { name, repo ? name, rev, owner, sha256, buildInputs ? [ ]
      , patches ? [ ] }:
      pkgs.callPackage ./builder.nix {
        inherit emacs name buildInputs patches;
        src = pkgs.fetchFromGitHub { inherit sha256 repo rev owner; };
      };

    withPatches = pkg: patches: pkg.overrideAttrs (attrs: { inherit patches; });
  };

  builder = pkgs.emacsPackagesNgGen emacs;

  emacsWithPackages =
    (builder.overrideScope' packages.overrides).emacsWithPackages
    packages.packages;

  customPathEntries =
    strings.concatStringsSep ":" [ "${requiredPrograms}/bin" ];
in pkgs.symlinkJoin {
  name = "emacs-wrapped";
  buildInputs = [ pkgs.makeWrapper ];
  paths = [ emacsWithPackages ];
  postBuild = ''
    for program in "$out/Applications/Emacs.app/Contents/MacOS/Emacs" "$out/bin/emacs"; do
      if [ -f "$program" ]; then
        wrapProgram "$program" \
          --prefix PATH ":" "${customPathEntries}" \
          --set NIX_EMACS_SRC_DIR "${emacs}/share/emacs/src/"
      fi
    done
  '';
}
