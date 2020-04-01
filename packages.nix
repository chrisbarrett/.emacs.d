{ pkgs, emacs }: epkgs:
with epkgs.elpaPackages; with epkgs.melpaPackages;
let
  # The versions of packages here are determined by `emacs-overlay`.
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
    cargo
    company
    company-anaconda
    company-auctex
    company-lsp
    counsel
    counsel-projectile
    csharp-mode
    csv-mode
    dante
    dap-mode
    dash
    dash-functional
    deadgrep
    deferred
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
    flycheck-rust
    forge
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
    omnisharp
    org
    org-bullets
    org-edna
    org-present
    ox-gfm
    page-break-lines
    paren-face
    pass
    pdf-tools
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
    volatile-highlights
    wgrep
    which-key
    world-time-mode
    ws-butler
    yaml-mode
    yasnippet
  ];

  helpers = pkgs.callPackage ./helpers.nix { inherit emacs; };
  inherit (helpers) github emacsmirror;

  extraPackages = {
    dired-plus = emacsmirror {
      name = "dired-plus";
      rev = "b51974b84b861592c3519117f3f51ee557ca01ba";
      sha256 = "0s59yd0axnja1zxc8snx013flf0f76n546i5ix5p0ngcbbhmm5kb";
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
    om = github {
      name = "om.el";
      owner = "ndwarshuis";
      rev = "5b3d6f2b326187cdd75b4590ba3a922b1288d726";
      sha256 = "158nc5k8fxdm9s97737z4syls41i2dmmlb28l7k85wgdhpyngszh";
      buildInputs = [ dash s org ];
      patches = [./patches/om/0001-eval-when-compile.patch];
    };
  };

in
fromOverlay ++ pkgs.lib.attrsets.attrValues extraPackages
