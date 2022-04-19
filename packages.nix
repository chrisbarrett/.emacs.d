{ emacsmirror, github, ... }: epkgs: with epkgs; [
  all-the-icons-ivy-rich
  auctex
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
  git-gutter
  git-gutter-fringe
  graphql-mode
  hcl-mode
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
  orderless
  ob-http
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
  org-superstar
  orgtbl-aggregate
  origami
  ox-gfm
  page-break-lines
  paren-face
  pass
  pcre2el
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
  string-inflection
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

  # KLUDGE: overlay version currently fails to build.
  (github {
    name = "org-transclusion";
    owner = "nobiot";
    rev = "ccc0aaa72732ea633bf52bcc8a0345cd3ac178fd";
    sha256 = "0sqs5gi58ngrjqpx35ff4q6lhgz32bcsxb9jkamw0rn18qp92w3p";
  })

  (github {
    name = "org-pretty-table";
    owner = "Fuco1";
    rev = "7bd68b420d3402826fea16ee5099d04aa9879b78";
    sha256 = "1lxs94318mvp49swrrb4p87b61qsy61ak8al3msqv1am69wyflsf";
  })

  (emacsmirror {
    name = "hide-comnt";
    rev = "d1e94f5152f20b2dc7b0d42898c1db37e5be57a6";
    sha256 = "002i9f97sq3jfknrw2nim1bhvj7xz3icviw7iffqmpmww4g1hq9l";
  })

  (emacsmirror {
    name = "info-plus";
    rev = "4a6b93c170169594e1e8ea60cd799a1a88a969da";
    sha256 = "1xzmx7m1qbl3b1x6yq1db1a108xqaa64ljfv1hdw763zmy4kc6m0";
  })
]
