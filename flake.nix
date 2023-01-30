{
  description = "Chris Barrett's Emacs configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/1d2409e";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, emacs-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        package = with pkgs; callPackage ./builders {
          texProgram = "${tectonic}/bin/tectonic";

          withPrograms = [
            (aspellWithDicts (ps: [ ps.en ]))
            delta
            multimarkdown
            nixpkgs-fmt
            ripgrep
            rustfmt
            sqlite

            # Language servers
            nodePackages.bash-language-server
            nodePackages.vscode-css-languageserver-bin
            nodePackages.vscode-html-languageserver-bin
            nodePackages.vscode-json-languageserver
            rnix-lsp
            rust-analyzer
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
            cider
            citar
            citar-org-roam
            clojure-mode
            consult
            corfu
            corfu-doc
            csharp-mode
            csv-mode
            dash
            deadgrep
            default-text-scale
            delight
            dockerfile-mode
            dogears
            doom-themes
            dumb-jump
            edit-indirect
            editorconfig
            eglot
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
            fira-code-mode
            flx
            forge
            format-all
            general
            git-auto-commit-mode
            git-gutter
            git-gutter-fringe
            gnuplot
            graphql-mode
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
            marginalia
            markdown-mode
            memoize
            messages-are-flowing
            mini-frame
            minions
            nix-mode
            no-littering
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
            proof-general
            rainbow-mode
            request
            rust-mode
            rotate
            shut-up
            simple-httpd
            smartparens
            smex
            string-inflection
            swift-mode
            terraform-mode
            ts
            typescript-mode
            undo-tree
            unfill
            use-package
            vertico
            volatile-highlights
            websocket
            wgrep
            which-key
            world-time-mode
            ws-butler
            yaml-mode
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
