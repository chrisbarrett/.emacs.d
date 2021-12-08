{ lib, github, emacsmirror, withPatches, ... }: {
  # Set the packages to bake in to emacs. The versions of packages are
  # determined by `emacs-overlay`, but can be overridden in the `overrides`
  # binding later in this file.
  packages = epkgs:
    with epkgs;
    let
      fromOverlay = [
        all-the-icons-ivy-rich
        auctex
        bibtex-completion
        browse-at-remote
        bufler
        company
        company-auctex
        consult
        csv-mode
        dash
        deadgrep
        default-text-scale
        delight
        direnv
        dockerfile-mode
        doom-themes
        dumb-jump
        edit-indirect
        editorconfig
        elisp-slime-nav
        emojify
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
        flycheck
        flycheck-ledger
        flycheck-package
        flycheck-plantuml
        forge
        format-all
        general
        git-auto-commit-mode
        graphql-mode
        hcl-mode
        helm-bibtex
        helpful
        hide-mode-line
        highlight-indent-guides
        highlight-thing
        historian
        hl-todo
        htmlize
        ibuffer-projectile
        json-mode
        latex-preview-pane
        ledger-mode
        link-hint
        lsp-mode
        lsp-ui
        magit
        magit-popup
        marginalia
        markdown-mode
        memoize
        messages-are-flowing
        mini-frame
        minions
        nix-mode
        no-littering
        orderless
        org
        org-cliplink
        org-contrib
        org-fragtog
        org-ml
        org-ql
        org-ref
        org-roam
        org-roam-bibtex
        org-roam-ui
        org-superstar
        orgtbl-aggregate
        ox-gfm
        page-break-lines
        paren-face
        pass
        pdf-tools
        plantuml-mode
        poporg
        popper
        projectile
        rainbow-mode
        request
        rotate
        selectrum
        shut-up
        simple-httpd
        smartparens
        smex
        terraform-mode
        ts
        typescript-mode
        undo-tree
        unfill
        use-package
        volatile-highlights
        websocket
        wgrep
        which-key
        world-time-mode
        ws-butler
        yaml-mode
        yasnippet
      ];

      extraPackages = rec {
        hide-comnt = emacsmirror {
          name = "hide-comnt";
          rev = "d1e94f5152f20b2dc7b0d42898c1db37e5be57a6";
          sha256 = "002i9f97sq3jfknrw2nim1bhvj7xz3icviw7iffqmpmww4g1hq9l";
        };

        info-plus = emacsmirror {
          name = "info-plus";
          rev = "4a6b93c170169594e1e8ea60cd799a1a88a969da";
          sha256 = "1xzmx7m1qbl3b1x6yq1db1a108xqaa64ljfv1hdw763zmy4kc6m0";
        };
      };
    in
    fromOverlay ++ lib.attrsets.attrValues extraPackages;

  # Apply any patches needed here.
  overrides = self: super: rec {
    markdown-mode = withPatches super.markdown-mode [ ./patches/markdown-mode.patch ];
  };
}
