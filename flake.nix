{
  outputs = { self, nixpkgs, emacs-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        };

        emacs = pkgs.emacs-unstable;

        lib = import ./lib.nix { inherit pkgs emacs; };

        emacsFromOverlay = pkgs.emacsWithPackagesFromUsePackage {
          package = emacs;
          config = ./init.el;

          extraEmacsPackages = epkgs: with epkgs; [
            shut-up
          ] ++ (map (name: builtins.getAttr name epkgs) (builtins.fromJSON (builtins.readFile ./ensured-packages.json)));

          override = final: prev: with lib.fetchers; {
            eglot-x = fromGithub {
              name = "eglot-x";
              owner = "nemethf";
              rev = "08cbd4369618e60576c95c194e63403f080328ba";
              sha256 = "sha256-cWicqHYR/XU+71a8OFgF8vc6dmT/Fy0EEgzX0xvYiDc=";
              buildInputs = [ final.eglot ];
            };

            hide-comnt = fromEmacsmirror {
              name = "hide-comnt";
              rev = "d1e94f5152f20b2dc7b0d42898c1db37e5be57a6";
              sha256 = "002i9f97sq3jfknrw2nim1bhvj7xz3icviw7iffqmpmww4g1hq9l";
            };

            info-plus = fromEmacsmirror {
              name = "info-plus";
              rev = "4a6b93c170169594e1e8ea60cd799a1a88a969da";
              sha256 = "1xzmx7m1qbl3b1x6yq1db1a108xqaa64ljfv1hdw763zmy4kc6m0";
            };
          };
        };

        pathExtras = with pkgs; lib.pkgsToPathString [
          (aspellWithDicts (ps: [ ps.en ]))
          graphviz-nox
          multimarkdown
          nixpkgs-fmt
          plantuml
          ripgrep
          rustfmt
          sqlite

          # Language servers
          marksman
          nodePackages.bash-language-server
          nodePackages.typescript-language-server
          nodePackages.vscode-css-languageserver-bin
          nodePackages.vscode-html-languageserver-bin
          nodePackages.vscode-json-languageserver
          rnix-lsp
          rust-analyzer
          terraform-ls
          vscode-extensions.dbaeumer.vscode-eslint
          vscode-extensions.graphql.vscode-graphql
          yaml-language-server
        ];

        package = with pkgs; symlinkJoin {
          name = "emacs-wrapped";
          buildInputs = [ makeWrapper ];
          paths = [ emacsFromOverlay ];
          postBuild = ''
            for program in "$out/Applications/Emacs.app/Contents/MacOS/Emacs" "$out/bin/emacs"; do
              if [ -f "$program" ]; then
                wrapProgram "$program" \
                  --prefix PATH ":" "${pathExtras}" \
                  --set NIX_EMACS_DARWIN_PATH_EXTRAS "${pathExtras}:/opt/homebrew/bin" \
                  --set NIX_EMACS_SRC_DIR "${emacs}/share/emacs" \
                  --set NIX_EMACS_TEX_PROGRAM "${tectonic}/bin/tectonic" \
                  --set NIX_EMACS_NODE_PROGRAM "${nodejs}/bin/node"
              fi
            done
          '';
        };
      in
      rec {
        packages.default = package;
        apps.default = flake-utils.lib.mkApp {
          drv = package;
          exePath = "/bin/emacs";
        };
        devShell = pkgs.mkShell {
          buildInputs = [ package pkgs.gnumake ];
        };
      });
}
