let
  # 2020-05-13
  overlayRev = "01ccf8565e3ef7236113758f547eb0f1f634e7cc";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${overlayRev}.tar.gz";
  });
in
{ pkgs ? import <nixpkgs> { overlays = [ emacs-overlay (import ./overlays) ];} }:

let
  # Additional programs to be injected into Emacs' environment.

  requiredPrograms = pkgs.symlinkJoin {
    name = "emacs-required-programs";
    paths = with pkgs; [
      (aspellWithDicts (ps: [ps.en]))
      htmlTidy
      multimarkdown
      nodejs
      ripgrep
      shellcheck
      tectonic
    ];
  };

  languageServers =  pkgs.callPackage ./language-servers {};

  # Build a custom Emacs version. It has a few fixes to make it work better with
  # yabai in macOS.
  emacs = pkgs.emacsGit.overrideAttrs (old: {
    withCsrc = true;

    patches = old.patches ++ [
      ./patches/emacs/0001-optional-org-gnus.patch
      ./patches/emacs/0002-fix-window-role.patch
      ./patches/emacs/0003-no-frame-refocus.patch
      ./patches/emacs/0004-no-titlebar.patch
      ./patches/emacs/0005-dont-warn-on-archives.patch
      ./patches/emacs/0006-prettier-ibuffer.patch
    ];

    postPatch = ''
      ${old.postPatch}

      # Delete the built-in orgmode.
      rm -r test/lisp/org lisp/org etc/org etc/ORG-NEWS doc/misc/org.texi
    '';
  });

  packages = pkgs.callPackage ./packages.nix rec {

    emacsmirror = args:
      github (args // { owner = "emacsmirror"; });

    github = { name, repo ? name, rev, owner, sha256, buildInputs ? [], patches ? [] }:
      pkgs.callPackage ./builder.nix {
        inherit emacs name buildInputs patches;
        src = pkgs.fetchFromGitHub {
          inherit sha256 repo rev owner;
        };
      };

    withPatches = pkg: patches:
      pkg.overrideAttrs (attrs: { inherit patches; });
  };

  builder = pkgs.emacsPackagesNgGen emacs;

  emacsWithPackages = (builder.overrideScope' packages.overrides).emacsWithPackages packages.packages;
in
pkgs.symlinkJoin {
  name = "emacs-wrapped";
  buildInputs = [pkgs.makeWrapper];
  paths = [emacsWithPackages];
  postBuild = ''
    for program in "$out/Applications/Emacs.app/Contents/MacOS/Emacs" "$out/bin/emacs"; do
      if [ -f "$program" ]; then
        wrapProgram "$program" \
          --set NIX_EMACS_PATH_EXTRAS "${languageServers}/bin:${requiredPrograms}/bin" \
          --set NIX_EMACS_LSP_ESLINT_NODE_PATH "${pkgs.nodejs}/bin/node" \
          --set NIX_EMACS_MU_BINARY "${pkgs.mu}/bin/mu" \
          --set NIX_PRETTIER_BINARY "${pkgs.prettier}/bin/prettier" \
          --prefix PATH ":" "${languageServers}/bin:${requiredPrograms}/bin" \
          --set NIX_EMACS_EMMY_LUA_JAR "${languageServers}/lib/emmy-lua.jar" \
          --set JAVA_HOME "${pkgs.jdk}"
      fi
    done
  '';

}
