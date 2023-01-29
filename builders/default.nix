{ pkgs, emacs, withPrograms, withLispPackages, texProgram, ... }:
let
  inherit (pkgs.lib) strings attrsets;

  fetchers = rec {
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
      pkgs.callPackage ./lisp-package.nix {
        inherit emacs name buildInputs patches preBuild;
        src = pkgs.fetchFromGitHub { inherit sha256 repo rev owner; };
      };
  };

  emacsEnv = (pkgs.emacsPackagesFor emacs).overrideScope' (pkgs.callPackage ../pkg-overrides.nix fetchers);

  pathExtras =
    let paths = map (pkg: "${pkg}/bin") withPrograms;
    in
    strings.concatStringsSep ":" paths;
in
pkgs.symlinkJoin
{
  name = "emacs-wrapped";
  buildInputs = [ pkgs.makeWrapper ];
  paths = [ (emacsEnv.emacsWithPackages withLispPackages) ];
  postBuild = ''
    for program in "$out/Applications/Emacs.app/Contents/MacOS/Emacs" "$out/bin/emacs"; do
      if [ -f "$program" ]; then
        wrapProgram "$program" \
          --prefix PATH ":" "${pathExtras}" \
          --set NIX_EMACS_DARWIN_PATH_EXTRAS "${pathExtras}:/opt/homebrew/bin" \
          --set NIX_EMACS_SRC_DIR "${emacs}/share/emacs" \
          --set NIX_EMACS_TEX_PROGRAM "${texProgram}"
      fi
    done
  '';
}
