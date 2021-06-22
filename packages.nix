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
        evil-magit
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
      ];

      extraPackages = {
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

        nano-emacs = github {
          name = "nano-emacs";
          owner = "rougier";
          rev = "0f5995602fb442856d50407d5122453595b1ad2a";
          sha256 = "1ml725v042q68r89l5ryx4f08cm81ng3pifijdfw62wnkdpdzpaj";
          buildInputs = [epkgs.ts epkgs.mini-frame];
        };
      };
    in fromOverlay ++ pkgs.lib.attrsets.attrValues extraPackages;

  # Apply any patches needed here.
  overrides = self: super: rec {
    counsel = withPatches super.counsel [ ./patches/counsel.patch ];
  };
}
