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
      ];

      extraPackages = {
        dired-plus = emacsmirror {
          name = "dired-plus";
          rev = "db4d82a6b1995a3aa31bd7f2dcaf9b83335d5576";
          sha256 = "10rfjf6gn5cx1kxq97xq7p24rnkw0hnzj32x4hny7bc6s3635d3x";
        };

        info-plus = emacsmirror {
          name = "info-plus";
          rev = "4a6b93c170169594e1e8ea60cd799a1a88a969da";
          sha256 = "1xzmx7m1qbl3b1x6yq1db1a108xqaa64ljfv1hdw763zmy4kc6m0";
        };
      };
    in fromOverlay ++ pkgs.lib.attrsets.attrValues extraPackages;

  # Apply any patches needed here.
  overrides = self: super: rec {
    counsel = withPatches super.counsel [ ./patches/counsel.patch ];
  };
}
