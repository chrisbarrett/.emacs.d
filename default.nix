let
  overlayRev = "84591bc733bef95d69bb07895ac7f8d34cd6b8a3";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${overlayRev}.tar.gz";
  });
in
{ pkgs ? import <nixpkgs> { overlays = [ emacs-overlay ]; } }:

let
  emacs = pkgs.emacsGit.overrideAttrs (old: {
    patches = old.patches ++ [
      ./patches/emacs/0001-optional-org-gnus.patch
      ./patches/emacs/0002-fix-window-role.patch
      ./patches/emacs/0003-no-frame-refocus.patch
      ./patches/emacs/0004-no-titlebar.patch
    ];
    postPatch = ''
      rm -rf .git
      # Delete the built-in orgmode.
      rm -r test/lisp/org lisp/org etc/org etc/ORG-NEWS doc/misc/org.texi
    '';
  });

  builder = pkgs.emacsPackagesNgGen emacs;
  packages = pkgs.callPackage ./packages.nix { emacs = emacs; };
in
builder.emacsWithPackages packages
