{ pkgs, emacs, ... }:
let
  inherit (pkgs.lib) strings attrsets;

  # Additional programs to be injected into Emacs' environment.

  requiredPrograms = pkgs.symlinkJoin {
    name = "emacs-required-programs";
    paths = with pkgs; [
      (aspellWithDicts (ps: [ ps.en ]))
      delta
      multimarkdown
      nixpkgs-fmt
      ripgrep
      rnix-lsp
      sqlite
      terraform-lsp
    ];
  };

  builders = pkgs.callPackage ./builders { };
  emacsEnv = (pkgs.emacsPackagesFor emacs).overrideScope' (pkgs.callPackage ./pkg-overrides.nix builders);
  packages = import ./packages.nix;
  customPathEntries = "${requiredPrograms}/bin";
in
pkgs.symlinkJoin
{
  name = "emacs-wrapped";
  buildInputs = [ pkgs.makeWrapper ];
  paths = [ (emacsEnv.emacsWithPackages packages) ];
  postBuild = ''
    for program in "$out/Applications/Emacs.app/Contents/MacOS/Emacs" "$out/bin/emacs"; do
      if [ -f "$program" ]; then
        wrapProgram "$program" \
          --prefix PATH ":" "${customPathEntries}" \
          --set NIX_EMACS_DARWIN_PATH_EXTRAS "${customPathEntries}:/opt/homebrew/bin" \
          --set NIX_EMACS_MU_BINARY "${pkgs.mu}/bin/mu" \
          --set NIX_EMACS_MU_LISP_DIR "${pkgs.mu}/share/emacs/site-lisp/mu4e" \
          --set NIX_EMACS_SRC_DIR "${emacs}/share/emacs" \
          --set NIX_EMACS_TEX_PROGRAM "${pkgs.tectonic}/bin/tectonic" \
          --set LSP_USE_PLISTS true
      fi
    done
  '';
}
