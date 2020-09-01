{ pkgs, github, emacsmirror, withPatches }: {
  # Set the packages to bake in to emacs. The versions of packages are
  # determined by `emacs-overlay`, but can be overridden in the `overrides`
  # binding later in this file.
  packages = epkgs:
    with epkgs;
    let
      fromOverlay = [
        aggressive-indent
        alert
        all-the-icons
        anaconda-mode
        annotate
        apiwrap
        async
        auctex
        auto-highlight-symbol
        browse-at-remote
        buttercup
        cargo
        clojure-mode
        company
        company-anaconda
        company-auctex
        company-org-roam
        counsel
        counsel-projectile
        csv-mode
        dante
        dap-mode
        dash
        dash-functional
        deadgrep
        default-text-scale
        deferred
        deft
        direnv
        docker
        dockerfile-mode
        doom-modeline
        doom-themes
        dumb-jump
        edit-indirect
        editorconfig
        el-patch
        elisp-slime-nav
        emmet-mode
        emojify
        evil
        evil-args
        evil-collection
        evil-iedit-state
        evil-magit
        evil-matchit
        evil-nerd-commenter
        evil-numbers
        evil-org
        evil-surround
        exec-path-from-shell
        f
        flx
        flycheck
        flycheck-ledger
        flycheck-package
        flycheck-plantuml
        flycheck-rust
        forge
        format-all
        general
        ghub
        git-auto-commit-mode
        git-gutter
        git-gutter-fringe
        git-timemachine
        graphql-mode
        graphviz-dot-mode
        groovy-mode
        haskell-mode
        helm-bibtex
        helpful
        hide-mode-line
        highlight-indent-guides
        highlight-thing
        historian
        hl-todo
        htmlize
        hydra
        ibuffer-projectile
        ivy
        ivy-historian
        json-mode
        latex-preview-pane
        ledger-mode
        link-hint
        lsp-java
        lsp-mode
        lsp-ui
        lua-mode
        magit
        major-mode-hydra
        markdown-mode
        memoize
        messages-are-flowing
        nameless
        nix-mode
        no-littering
        noflet
        nvm
        ob-restclient
        org
        org-bullets
        org-edna
        org-noter
        org-present
        org-roam
        org-roam-bibtex
        ox-gfm
        page-break-lines
        paren-face
        pass
        pdf-tools
        plantuml-mode
        pip-requirements
        poporg
        pretty-hydra
        projectile
        protobuf-mode
        py-isort
        py-yapf
        pytest
        pyvenv
        rainbow-mode
        restclient
        rotate
        rust-mode
        s
        sbt-mode
        scala-mode
        smartparens
        swiper
        terraform-mode
        toml-mode
        typescript-mode
        undo-tree
        unfill
        use-package
        verb
        volatile-highlights
        wgrep
        which-key
        world-time-mode
        ws-butler
        yaml-mode
        yasnippet
      ];

      extraPackages = {
        dired-plus = emacsmirror {
          name = "dired-plus";
          rev = "db4d82a6b1995a3aa31bd7f2dcaf9b83335d5576";
          sha256 = "10rfjf6gn5cx1kxq97xq7p24rnkw0hnzj32x4hny7bc6s3635d3x";
        };
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
        literate-calc-mode = github {
          name = "literate-calc-mode.el";
          owner = "chrisbarrett";
          rev = "100ac2719566c1a85cd202bd7a9b3e015e9d27d5";
          sha256 = "129caqfmv0ybaa3iqfiryc28p327iihxxgbf0ryh9v31djb8jqx4";
          buildInputs = [ s ];
        };
        lsp-lua-emmy = github {
          name = "lsp-lua-emmy";
          owner = "phenix3443";
          rev = "ab53fb2a8b8942804eb75bab5624fd19f1d360bf";
          sha256 = "0rbizis73n1w5ig07fj1han74chbc1zpbp5bn37rj0gz608aqka8";
          buildInputs = [ lsp-mode ];
        };
        om = github {
          name = "om.el";
          owner = "ndwarshuis";
          rev = "5b3d6f2b326187cdd75b4590ba3a922b1288d726";
          sha256 = "158nc5k8fxdm9s97737z4syls41i2dmmlb28l7k85wgdhpyngszh";
          buildInputs = [ dash s org ];
          patches = [ ./patches/om.patch ];
        };
      };
    in fromOverlay ++ pkgs.lib.attrsets.attrValues extraPackages;

  # Apply any patches needed here.
  overrides = self: super: rec {

    counsel = withPatches super.counsel [ ./patches/counsel.patch ];

    doom-modeline =
      withPatches super.doom-modeline [ ./patches/doom-modeline-core.patch ];

    evil = withPatches super.evil [ ./patches/evil-commands.patch ];

    ledger-mode =
      withPatches super.ledger-mode [ ./patches/ledger-report.patch ];

    org-edna = withPatches (github {
      name = "org-edna";
      owner = "akirak";
      rev = "8824dc5d9f9fa3859fa24d539ee27a893eaa77ab";
      sha256 = "1gk0v572gwid9y32wwd80m496sqj2765ny2xbi01zsr84xxbvb59";
      buildInputs = [ super.org ];
    }) [ ./patches/org-edna.patch ];

    messages-are-flowing = withPatches super.messages-are-flowing
      [ ./patches/messages-are-flowing.patch ];

    pass = withPatches super.pass [ ./patches/pass.patch ];
  };
}
