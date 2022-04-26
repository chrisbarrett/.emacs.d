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
  revs = builtins.fromJSON (builtins.readFile ./emacs-overlay.json);
in
{
  # Version of nixpkgs used for building binaries and Emacs itself.
  pkgs ? nixpkgsWithOverlays {
    emacsOverlayRev = revs.pkgs;
  }
  # Version of nixpkgs that determines 3rd-party Lisp package versions.
, lispPkgs ? nixpkgsWithOverlays {
    emacsOverlayRev = revs.lisp;
  }
, emacs ? pkgs.emacsNativeComp
}:

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
      sqlite
    ];
  };

  languageServers = with pkgs; {
    bash = nodePackages.bash-language-server;
    css = nodePackages.vscode-css-languageserver-bin;
    eslint = vscode-extensions.dbaeumer.vscode-eslint;
    graphql = vscode-extensions.graphql.vscode-graphql;
    html = nodePackages.vscode-html-languageserver-bin;
    json = nodePackages.vscode-json-languageserver;
    nix = rnix-lsp;
    terraform = terraform-lsp;
    typescript = nodePackages.typescript-language-server;
    yaml = nodePackages.yaml-language-server;
  };

  builders = pkgs.callPackage ./builders { };
  packages = pkgs.callPackage ./packages.nix builders;
  emacsEnv = (lispPkgs.emacsPackagesFor emacs).overrideScope' (import ./patches);

  customPathEntries =
    let paths = [ "${requiredPrograms}/bin" ] ++ (map (pkg: "${pkg}/bin") (attrsets.attrValues languageServers));
    in
    strings.concatStringsSep ":" paths;
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
          --set NIX_EMACS_DARWIN_PATH_EXTRAS "${customPathEntries}" \
          --set NIX_EMACS_ESLINT_SERVER_SCRIPT "${languageServers.eslint}/lib/eslintServer.js" \
          --set NIX_EMACS_LSP_ESLINT_NODE_PATH "${pkgs.nodejs}/bin/node" \
          --set NIX_EMACS_TS_LANGUAGE_SERVER "${languageServers.typescript}/bin/typescript-language-server" \
          --set NIX_EMACS_MU_BINARY "${pkgs.mu}/bin/mu" \
          --set NIX_EMACS_MU_LISP_DIR "${pkgs.mu}/share/emacs/site-lisp/mu4e" \
          --set NIX_EMACS_SRC_DIR "${emacs}/share/emacs" \
          --set NIX_EMACS_TEX_PROGRAM "${pkgs.tectonic}/bin/tectonic"
      fi
    done
  '';
}
