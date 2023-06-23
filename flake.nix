{
  description = "Chris Barrett's Emacs configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, emacs-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        };

        package = with pkgs; callPackage ./builders {
          emacs = emacsUnstable;
          nodeProgram = "${nodejs}/bin/node";
          texProgram = "${tectonic}/bin/tectonic";

          withPrograms = [
            (aspellWithDicts (ps: [ ps.en ]))
            delta
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

          withLispPackages = epkgs: with epkgs; [
            all-the-icons
            applescript-mode
            auctex
            browse-at-remote
            bufler
            cape
            chatgpt-shell
            cider
            citar
            citar-org-roam
            clojure-mode
            consult
            corfu
            csv-mode
            dall-e-shell
            dash
            deadgrep
            default-text-scale
            delight
            diredfl
            dirvish
            dockerfile-mode
            dogears
            doom-themes
            dumb-jump
            edit-indirect
            editorconfig
            eglot
            eglot-x
            elisp-slime-nav
            embark
            embark-consult
            emojify
            envrc
            evil
            evil-args
            evil-collection
            evil-iedit-state
            evil-matchit
            evil-nerd-commenter
            evil-numbers
            evil-org
            evil-surround
            f
            flx
            forge
            format-all
            general
            git-auto-commit-mode
            git-gutter
            git-gutter-fringe
            gnuplot
            graphql-mode
            ligature
            groovy-mode
            hcl-mode
            helpful
            hide-comnt
            hide-mode-line
            highlight-indent-guides
            highlight-thing
            historian
            hl-todo
            htmlize
            info-plus
            iscroll
            json-mode
            kind-icon
            latex-preview-pane
            link-hint
            magit
            magit-delta
            magit-popup
            magit-todos
            marginalia
            markdown-mode
            memoize
            messages-are-flowing
            mini-frame
            minions
            nix-mode
            no-littering
            ob-chatgpt-shell
            ob-http
            orderless
            org
            org-appear
            org-cliplink
            org-contrib
            org-download
            org-drill
            org-fragtog
            org-ml
            org-ql
            org-roam
            org-roam-ui
            org-super-agenda
            org-superstar
            org-transclusion
            orgtbl-aggregate
            origami
            ox-gfm
            page-break-lines
            paren-face
            pcmpl-args
            pcre2el
            pdf-tools
            plantuml-mode
            poporg
            prettier
            proof-general
            rainbow-mode
            request
            rust-mode
            rustic
            rotate
            shut-up
            simple-httpd
            smartparens
            smex
            string-inflection
            swift-mode
            terraform-mode
            ts
            undo-tree
            unfill
            vertico
            volatile-highlights
            vscode-icon
            websocket
            wgrep
            which-key
            world-time-mode
            ws-butler
            yasnippet
          ];
        };
      in
      with pkgs;
      rec {
        packages.default = package;

        apps.default = flake-utils.lib.mkApp {
          drv = package;
          exePath = "/bin/emacs";
        };

        devShell = mkShell {
          buildInputs = [ package gnumake ];
        };
      });
}
