{ pkgs, lib, github, emacsmirror, withPatches }: {
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
        counsel
        counsel-projectile
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
        ivy
        ivy-historian
        json-mode
        latex-preview-pane
        ledger-mode
        link-hint
        magit
        magit-popup
        markdown-mode
        memoize
        messages-are-flowing
        mini-frame
        minions
        nix-mode
        no-littering
        orderless
        org
        org-bullets
        org-cliplink
        org-contrib
        org-ml
        org-ql
        org-ref
        org-roam
        org-roam-bibtex
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
        rotate
        shut-up
        smartparens
        smex
        terraform-mode
        tide
        ts
        typescript-mode
        undo-tree
        unfill
        use-package
        volatile-highlights
        wgrep
        which-key
        world-time-mode
        ws-butler
        yaml-mode
        yasnippet
        websocket
        simple-httpd
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

        org-roam-ui = github {
          name = "org-roam-ui";
          owner = "org-roam";
          rev = "ce08e92ef245961c25d1b0febb5983fcc5c2809e";
          sha256 = "0pqz8pp2v0jcjzpyrkckbj6ha9lvi219z5qaa73hn9206ayiy4f0";
          buildInputs = [ org-roam f websocket simple-httpd ];
          postInstall = ''
            SITE_LISP="$out/share/emacs/site-lisp"
            DEST_DIR="$SITE_LISP/elpa/org-roam-ui"

            mkdir -p "$DEST_DIR"
            mv "$SITE_LISP/org-roam-ui".* "$DEST_DIR"
            cp -r out "$DEST_DIR/out"
          '';
        };
      };
    in
    fromOverlay ++ lib.attrsets.attrValues extraPackages;

  # Apply any patches needed here.
  overrides = self: super: rec {
    counsel = withPatches super.counsel [ ./patches/counsel.patch ];
    markdown-mode = withPatches super.markdown-mode [ ./patches/markdown-mode.patch ];

    # KLUDGE: yaml from melpa fails to build, so we need to supply our own
    # version.
    yaml = github {
      name = "yaml.el";
      owner = "zkry";
      rev = "84b88c9ed178af16da18b230c1f61c57cefedf28";
      sha256 = "13fjxrr7iyfagbm21p5is5jw1zv56ns2mnac145v8lqli6mrr5gx";
    };
  };
}
