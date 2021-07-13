{ pkgs, github, emacsmirror, withPatches }: {
  # Set the packages to bake in to emacs. The versions of packages are
  # determined by `emacs-overlay`, but can be overridden in the `overrides`
  # binding later in this file.
  packages = epkgs:
    with epkgs;
    let
      fromOverlay = [
        delight
        shut-up
        use-package
        dockerfile-mode
        general
        no-littering
        f
        dash
        world-time-mode
        direnv
        evil
        link-hint
        undo-tree
        evil-surround
        evil-collection
        evil-args
        evil-matchit
        evil-numbers
        evil-iedit-state
        evil-nerd-commenter
        company
        default-text-scale
        editorconfig
        aggressive-indent
        ws-butler
        unfill
        volatile-highlights
        highlight-thing
        terraform-mode
        hcl-mode
        yaml-mode
        highlight-indent-guides
        json-mode
        pdf-tools
        hl-todo
        dumb-jump
        deadgrep
        rotate
        ivy
        flx
        pass
        graphql-mode
        doom-themes
        format-all
        smex
        counsel
        historian
        ivy-historian
        elisp-slime-nav
        helpful
        which-key
        rainbow-mode
        flycheck
        flycheck-package
        magit
        forge
        git-auto-commit-mode
        nix-mode
        markdown-mode
        edit-indirect
        ibuffer-projectile
        smartparens
        paren-face
        auctex
        company-auctex
        latex-preview-pane
        ledger-mode
        flycheck-ledger
        page-break-lines
        plantuml-mode
        flycheck-plantuml
        projectile
        counsel-projectile
        org
        mini-frame
        yasnippet
        typescript-mode
        tide
        messages-are-flowing
        hide-mode-line
        magit-popup
        poporg
        emojify
        org-bullets
        htmlize
        evil-org
        org-ref
        org-roam
        ox-gfm
        minions
        memoize
        deft
        helm-bibtex
        bibtex-completion
        org-roam-bibtex
        popper
      ];

      extraPackages = rec {
        dired-plus = emacsmirror {
          name = "dired-plus";
          rev = "91ce389584b766985efe5821bf0d4143d9cd965b";
          sha256 = "179kmv2za7ajq4b3aqpl83ny1dkvplf1bp65dqyyk9bc3jqli97p";
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

        mu4e-dashboard = github {
          name = "mu4e-dashboard";
          owner = "rougier";
          rev = "40b2d48da55b7ac841d62737ea9cdf54e8442cf3";
          sha256 = "1i94gdyk9f5c2vyr184znr54cbvg6apcq38l2389m3h8lxg1m5na";
          buildInputs = [ pkgs.mu ];
        };

        mu4e-thread-folding = github {
          name = "mu4e-thread-folding";
          owner = "rougier";
          rev = "c6915585263a744b4da4a0e334393150603136dc";
          sha256 = "0fki9506q42fz6a86pnx2ll3kl25d6nh4b735c323abnwjirjd50";
          buildInputs = [ pkgs.mu ];
        };

        org-noter = github {
          name = "org-noter";
          owner = "chrisbarrett";
          rev = "6c2c1a91c1830e03110487867734137f5698904a";
          sha256 = "1gvalalk27kvvm1ysj8vpjdky3sfw8jvympv9vkkr3dby8b2apwa";
          buildInputs = [ org ];
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
    in
    fromOverlay ++ pkgs.lib.attrsets.attrValues extraPackages;

  # Apply any patches needed here.
  overrides = self: super: rec {
    counsel = withPatches super.counsel [ ./patches/counsel.patch ];
  };
}
