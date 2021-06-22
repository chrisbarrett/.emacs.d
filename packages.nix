{ pkgs, github, emacsmirror, withPatches }: {
  # Set the packages to bake in to emacs. The versions of packages are
  # determined by `emacs-overlay`, but can be overridden in the `overrides`
  # binding later in this file.
  packages = epkgs:
    with epkgs;
    let
      fromOverlay = [
        shut-up
        use-package
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
        company
        default-text-scale
        editorconfig
        aggressive-indent
        ws-butler
        unfill
        volatile-highlights
        highlight-thing
        dumb-jump
        deadgrep
        rotate
        ivy
        flx
        smex
        counsel
        historian
        ivy-historian
        elisp-slime-nav
        helpful
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
        auctex
        company-auctex
        latex-preview-pane
        ledger-mode
        flycheck-ledger
        plantuml-mode
        flycheck-plantuml
        projectile
        counsel-projectile
        org
        mini-frame
      ];

      extraPackages = rec {
        dired-plus = emacsmirror {
          name = "dired-plus";
          rev = "91ce389584b766985efe5821bf0d4143d9cd965b";
          sha256 = "179kmv2za7ajq4b3aqpl83ny1dkvplf1bp65dqyyk9bc3jqli97p";
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
          buildInputs = [pkgs.mu];
        };

        mu4e-thread-folding = github {
          name = "mu4e-thread-folding";
          owner = "rougier";
          rev = "c6915585263a744b4da4a0e334393150603136dc";
          sha256 = "0fki9506q42fz6a86pnx2ll3kl25d6nh4b735c323abnwjirjd50";
          buildInputs = [pkgs.mu];
        };

        nano-emacs = github {
          name = "nano-emacs";
          owner = "rougier";
          rev = "0f5995602fb442856d50407d5122453595b1ad2a";
          sha256 = "1ml725v042q68r89l5ryx4f08cm81ng3pifijdfw62wnkdpdzpaj";
          buildInputs = with epkgs; [
            smex
            ts
            mini-frame
            svg-tag-mode
            mu4e-dashboard
            mu4e-thread-folding
            pkgs.mu
          ];
          preBuild = ''
            # Unneeded file that fails native compilation.
            rm nano.el
          '';
        };
      };
    in fromOverlay ++ pkgs.lib.attrsets.attrValues extraPackages;

  # Apply any patches needed here.
  overrides = self: super: rec {
    counsel = withPatches super.counsel [ ./patches/counsel.patch ];
  };
}
