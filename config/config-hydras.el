;;; config-hydras.el --- Main hydra definitions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'buffer-cmds)
(require 'jump-cmds)
(require 'org-funcs)
(require 'window-cmds)

(autoload 'counsel-find-file "config-ivy")
(autoload 'counsel-recentf "config-ivy")
(autoload 'evil-window-next "evil")
(autoload 'evil-window-prev "evil")
(autoload 'evil-window-rotate-downwards "evil")
(autoload 'generate-password/body "generate-password")
(autoload 'ivy-switch-buffer "ivy")
(autoload 'ledger-import "ledger-import")
(autoload 'ledger-report "ledger-mode" nil t)
(autoload 'neotree-toggle "neotree")
(autoload 'org-capture-goto-last-stored "org-capture")
(autoload 'org-clock/body "org-hydras")
(autoload 'org-narrow-to-subtree "org")
(autoload 'org-ref-bibtex-hydra/body "org-ref-bibtex")
(autoload 'profiler-report "profiler")
(autoload 'profiler-stop "profiler")

(eval-and-compile
  (defun hydra-title-with-octicon (icon title)
    (concat (all-the-icons-octicon icon :face 'all-the-icons-orange :v-adjust 0.05)
            " " title
            "\n"))

  (defun hydra-title-with-fileicon (icon title)
    (concat (all-the-icons-fileicon icon :face 'all-the-icons-orange :v-adjust -0.15)
            " " title
            "\n"))

  (defun hydra-title-with-mode-icon (mode title)
    (concat (all-the-icons-icon-for-mode mode)
            " " title
            "\n"))

  (defun hydra-title-with-faicon (icon title)
    (concat (all-the-icons-faicon icon :face 'all-the-icons-orange :v-adjust 0.05)
            " " title
            "\n"))

  (defun hydra-title-with-aicon (icon title)
    (concat (all-the-icons-alltheicon icon :face 'all-the-icons-orange :v-adjust 0.05)
            " " title
            "\n"))

  (defun hydra-title-with-mat-icon (icon title)
    (concat (all-the-icons-material icon :face 'all-the-icons-orange)
            " " title
            "\n")))

(use-package all-the-icons)

(use-package pretty-hydra
  :demand t
  :preface
  (defun config-hydras--add-quit-bindings (result)
    (append '(("q" nil :exit t)
              ("<escape>" nil :exit t))
            result))
  :config
  (advice-add #'pretty-hydra--get-heads :filter-return #'config-hydras--add-quit-bindings))


;; Hydra definitions

(eval-when-compile
  (require 'all-the-icons)
  (require 'pretty-hydra))

(pretty-hydra-define comments
  (:hint nil
   :color teal
   :title (hydra-title-with-octicon "comment" "Comments"))
  ("Toggle Comments"
   (("l" evilnc-comment-or-uncomment-lines "lines")
    ("r" comment-or-uncomment-region "region")
    ("p" evilnc-comment-or-uncomment-paragraphs "paragraphs")
    ("s" (progn (sp-mark-sexp) (call-interactively #'comment-region)) "sexp"))
   "With Copy"
   (("y" evil-funcs/copy-and-comment-lines "copy"))))

(pretty-hydra-define font-scale
  (:hint nil
   :color amaranth
   :title (hydra-title-with-faicon "search-plus" "Font Scale"))
  (""
   (("+" (default-text-scale-increase) "zoom in")
    ("-" (default-text-scale-decrease) "zoom out")
    ("0" (default-text-scale-reset) "reset"))))

(pretty-hydra-define buffers
  (:hint nil
   :color teal
   :title (hydra-title-with-faicon "files-o" "Buffer Commands"))
  ("Switch"
   (("n" next-buffer "next" :exit nil)
    ("p" previous-buffer "back" :exit nil )
    ("l" ibuffer "list")
    ("s" ivy-switch-buffer "switch..."))

   "Manage"
   (("b" bury-buffer "bury")
    ("d" kill-current-buffer "kill")
    ("w" save-buffer "save")
    ("v" reload-file "reload"))))

(pretty-hydra-define windows
  (:hint nil
   :color teal
   :title (hydra-title-with-faicon "clone" "Window Management"))
  ("Switch"
   (("w" evil-window-next "next")
    ("n" evil-window-prev "forward" :exit nil)
    ("p" evil-window-prev "back" :exit nil)
    ("r" evil-window-rotate-downwards "rotate"))

   "Split"
   (("/" window-cmds-split-horizontally "vertical")
    ("-" window-cmds-split-vertically "horizontal")
    ("=" balance-windows "rebalance"))

   "Close"
   (("d" delete-window "window")
    ("o" delete-other-windows "others"))

   "Toggles"
   (("SPC" window-cmds-toggle-current-window-dedication "toggle dedicated"))))

(pretty-hydra-define files
  (:hint nil
   :color teal
   :title (hydra-title-with-faicon "hdd-o" "File Commands"))
  ("Find"
   (("f" counsel-find-file "find file...")
    ("l" find-file-literally "find file... (literally)")
    ("o" find-file-other-window "find file... (other window)")
    ("p" find-file-at-point "at pt...")
    ("h" hexl-find-file "as hex...")
    ("r" counsel-recentf "recent..."))

   "Save"
   (("s" save-buffer "buffer")
    ("S" save-some-buffers "many buffers")
    ("W" write-file "write copy...")
    ("R" rename-file-and-buffer "rename...")
    ("D" delete-current-buffer-and-file "delete"))

   "Copy"
   (("d" copy-buffer-directory "dir")
    ("y" copy-buffer-path "path")
    ("Y" copy-buffer-name "filename"))

   "Other"
   (("e" sudo-edit "edit with sudo...")
    ("v" reload-file "reload"))))

(pretty-hydra-define errors
  (:hint nil
   :color teal
   :title (hydra-title-with-mat-icon "error_outline" "Errors"))
  ("Navigation"
   (("n" flycheck-next-error "next" :color red)
    ("p" flycheck-previous-error "previous" :color red)
    ("l" config-flycheck-toggle-error-list "list errors"))

   "Actions"
   (("r" flycheck-buffer "run checks")
    ("c" flycheck-clear "clear")
    ("e" flycheck-explain-error-at-point "explain"))

   "Checkers"
   (("h" flycheck-describe-checker "describe...")
    ("s" flycheck-select-checker "select...")
    ("v" flycheck-verify-setup "verify"))))

(pretty-hydra-define help
  (:hint nil
   :color teal
   :help nil
   :title (hydra-title-with-mat-icon "help_outline" "Help"))
  ("Docs"
   (("i" info "info")
    ("m" man "man..."))

   "Describe"
   (("dc" counsel-describe-face "face...")
    ("dC" helpful-command "command...")
    ("df" helpful-callable "function...")
    ("dk" helpful-key "key...")
    ("dm" describe-mode "mode...")
    ("dp" describe-text-properties "text-props...")
    ("dv" helpful-variable "variable..."))

   "Find"
   (("fc" find-face-definition "face...")
    ("ff" find-function "function...")
    ("fl" counsel-find-library "library...")
    ("fv" find-variable "variable..."))))

(pretty-hydra-define kill
  (:hint nil
   :color teal
   :title (hydra-title-with-mat-icon "close" "Kill"))
  ("Kill"
   (("b" #'kill-this-buffer "buffer")
    ("w" #'delete-window "window"))
   "Kill-Ring"
   (("r" counsel-yank-pop "browse..."))))

(pretty-hydra-define narrowing
  (:hint nil
   :color teal
   :title (hydra-title-with-mat-icon "photo_size_select_small" "Narrowing"))
  ("Narrow to..."
   (("f" #'narrow-to-defun "function")
    ("r" #'narrow-to-region "region")
    ("s" #'org-narrow-to-subtree "org subtree")
    ("S" #'org-tree-to-indirect-buffer))
   "Actions"
   (("w" #'widen "widen"))))

(pretty-hydra-define ledger
  (:hint nil
   :color teal
   :title (hydra-title-with-mode-icon 'ledger-mode "Ledger"))
  ("Goto"
   (("a" (find-file (f-join paths-ledger-directory "accounts.dat")) "accounts")
    ("b" (find-file (f-join paths-ledger-directory "budget.ledger")) "budget")
    ("f" (find-file (f-join paths-ledger-directory "flat.ledger")) "flat")
    ("i" (find-file (f-join paths-ledger-directory "lisp" "init.el")) "lisp init file")
    ("j" (find-file (f-join paths-ledger-directory "journal.ledger")) "journal")
    ("m" (find-file (f-join paths-ledger-directory "master.ledger")) "master file"))
   "Actions"
   (("c" #'ledger-import "import CSV..."))))

(pretty-hydra-define org
  (:hint nil
   :color teal
   :title (hydra-title-with-mode-icon 'org-mode "Org"))
  ("Actions"
   (("SPC" deft "edit/create note...")
    ("a" org-funcs-agenda-dwim "show agenda...")
    ("c" org-clock/body "clock...")
    ("k" org-capture "capture...")
    ("l" org-store-link "store link...")
    ("s" org-search-view "search..."))

   "Roam"
   (("<tab>" org-roam "Roam backlinks")
    ("f" org-roam-find-file "roam file...")
    ("I" org-roam-jump-to-index "Go to Index")
    ("n" org-funcs-dailies-today "Dailies: today")
    ("y" org-roam-dailies-yesterday "Dailies: yesterday")
    ("d" org-roam-dailies-date "Dialies: date..."))

   "Refs"
   (("b" helm-bibtex "bibliography")
    ("r" org-ref-bibtex-hydra/body "references"))

   "Goto"
   (("g" org-capture-goto-last-stored "last captured item")
    ("t" org-funcs-todo-list "todo list")
    ("v" org-tags-view "tags")
    ("w" org-funcs-goto-work "work"))))

(pretty-hydra-define project
  (:hint nil
   :color teal
   :title (hydra-title-with-octicon "repo" "Project"))
  ("Actions"
   (("!" projectile-run-async-shell-command-in-root "shell command...")
    ("c" projectile-compile-project "compile...")
    ("u" projectile-run-project "run...")
    ("t" config-projectile-test-project "test..."))

   "Open"
   (("p" counsel-projectile-switch-project "project...")
    ("f" counsel-projectile-find-file "file...")
    ("d" counsel-projectile-find-dir "directory...")
    ("b" counsel-projectile-switch-to-buffer "buffer..."))

   "Navigate"
   (("<tab>" projectile-toggle-between-implementation-and-test "test/impl")
    ("<backtab>" projectile-find-implementation-or-test-other-window "test/impl (other window)")
    ("D" projectile-dired "project root (dired)"))

   "Search/Replace"
   (("/" counsel-projectile-rg "search...")
    ("r" projectile-replace "replace..."))))

(pretty-hydra-define parens
  (:hint nil
   :color teal
   :title (hydra-title-with-mat-icon "code" "Smartparens"))
  ("Navigation"
   (("h" sp-beginning-of-sexp "beginning")
    ("l" sp-end-of-sexp "end")
    ("n" sp-next-sexp "next")
    ("p" sp-previous-sexp "previous")
    ("<" sp-backward-up-sexp "up (start)")
    (">" sp-up-sexp "up (end)"))
   "Killing"
   (("c" sp-convolute-sexp "convolute")
    ("D" sp-backward-kill-sexp "kill back")
    ("d" sp-kill-sexp "kill forward")
    ("K" sp-splice-sexp-killing-backward "splice back")
    ("k" sp-splice-sexp-killing-forward "splice forward")
    ("s" sp-splice-sexp-killing-around "splice around")
    ("r" sp-raise-sexp "raise"))
   "Wrapping"
   (("A" sp-add-to-previous-sexp "add to previous")
    ("a" sp-add-to-next-sexp "add to next")
    ("B" sp-backward-barf-sexp "barf back")
    ("b" sp-forward-barf-sexp "barf forward")
    ("M" sp-backward-slurp-sexp "slurp back")
    ("m" sp-forward-slurp-sexp "slurp forward")
    ("e" sp-emit-sexp "emit")
    ("j" sp-join-sexp "join"))
   ""
   (("t" sp-transpose-sexp "transpose")
    ("U" sp-backward-unwrap-sexp "unwrap back")
    ("u" sp-unwrap-sexp "unwrap forward")
    ("w" sp-rewrap-sexp "rewrap")
    ("x" sp-split-sexp "split")
    ("Y" sp-backward-copy-sexp "copy back")
    ("y" sp-copy-sexp "copy"))))

(pretty-hydra-define toggles
  (:hint nil
   :color teal
   :title (hydra-title-with-faicon "toggle-on" "Toggles"))
  (""
   (("t" config-themes-toggle "theme")
    ("i" toggle-input-method "input method")
    ("c" hide/show-comments-toggle "comments")
    ("m" global-hide-mode-line-mode "mode-line"))))

(pretty-hydra-define yasnippet
  (:hint nil
   :color teal
   :title (hydra-title-with-mat-icon "content_copy" "Snippets"))
  (""
   (("n" #'yas-new-snippet "new")
    ("e" #'yas-expand "expand"))
   ""
   (("f" #'yas-visit-snippet-file "visit file...")
    ("y" #'yas-insert-snippet "insert..."))))

;; Git hydras

(pretty-hydra-define git-and-files
  (:hint nil
   :color teal
   :title (hydra-title-with-aicon "git" "Git and Goto"))
  ("Goto"
   (("c" jump-to-config-file "config file...")
    ("i" jump-to-init-file "init file")
    ("n" jump-to-nix-config "nix config file")
    ("S" jump-to-nix-system-config "nix system config")
    ("?" jump-to-messages "messages"))
   ""
   (("p" jump-to-personal-config "personal config")
    ("H" jump-to-host-file "host file")
    ("u" jump-to-package-usage "package usage"))

   "Git"
   (("s" magit-status "magit")
    ("d" config-git-diff-buffer-file "diff buffer...")
    ("b" magit-blame "blame...")
    ("f" config-git-find-file "find file...")
    ("r" browse-at-remote "browse at remote"))
   ""
   (("h" git-hunks/body "navigate hunks")
    ("l" magit-log-buffer-file "log buffer")
    ("t" git-time-machine/body "time machine")
    ("w" magit-worktree-status "worktree status")
    ("W" magit-worktree "worktree..."))

   "Symbol Definition"
   (("g" jump-to-definition "symbol/type")
    ("o" jump-to-definition-other-window "symbol/type (other window)")
    ("m" lsp-goto-implementation "symbol implementation")
    ("SPC" pop-tag-mark "jump back"))))

(pretty-hydra-define git-time-machine
  (:foreign-keys run
   :pre (unless (bound-and-true-p git-timemachine-mode)
          (call-interactively 'git-timemachine))
   :post (when (bound-and-true-p git-timemachine-mode)
           (git-timemachine-quit))
   :title (hydra-title-with-aicon "git" "Git Time Machine"))
  ("Step"
   (("p" git-timemachine-show-previous-revision "previous")
    ("n" git-timemachine-show-next-revision "next"))
   "Goto Rev"
   (("h" git-timemachine-show-current-revision "HEAD")
    ("g" git-timemachine-show-nth-revision "by number"))
   "Actions"
   (("Y" git-timemachine-kill-revision "copy sha"))))

(pretty-hydra-define git-hunks
  (:foreign-keys run
   :hint nil
   :color red
   :title (hydra-title-with-aicon "git" "Git Hunks"))
  ("Navigate"
   (("n" diff-hl-next-hunk "next")
    ("p" diff-hl-previous-hunk "previous"))
   "Actions"
   (("d" diff-hl-diff-goto-hunk "show diff" :exit t)
    ("x" diff-hl-revert-hunk "revert"))))

;; Application hydras

(pretty-hydra-define applications
  (:hint nil
   :color teal
   :title (hydra-title-with-mat-icon "apps" "Applications"))
  ("Productivity"
   (("c" quick-calc "quick calc...")
    ("C" full-calc "calc")
    ("m" mu4e "mu4e")
    ("p" pass "password-store")
    ("P" generate-password/body "generate password...")
    ("w" world-time-list "world clock"))

   "Emacs"
   (("r" profiler/body "profiler...")
    ("v" prodigy "services..."))

   "Shells"
   (("t" vterm-toggle "terminal")
    ("n" nix-repl-show "nix-repl"))

   "Other"
   (("l" (with-current-buffer (find-file-noselect (f-join paths-ledger-directory "master.ledger"))
           (call-interactively #'ledger-report))
     "ledger report..."))))

(pretty-hydra-define profiler
  (:hint nil
   :color teal
   :title (hydra-title-with-faicon "bar-chart" "Profiler"))
  (""
   (("p" profiler-start "start...")
    ("s" profiler-stop "stop")
    ("r" profiler-report "report"))))

(pretty-hydra-define language-server
  (:hint nil
   :color teal
   :title (hydra-title-with-octicon "code" "Language Server"))
  ("Navigate"
   (("n" lsp-ui-find-next-reference "next ref" :exit nil)
    ("N" lsp-ui-find-prev-reference "prev ref" :exit nil)
    ("p" lsp-ui-find-prev-reference "prev ref" :exit nil))
   "Actions"
   (("R" lsp-rename "rename symbol")
    ("f" (lambda () (interactive) (lsp-format-buffer) (save-buffer)) "format buffer")
    ("o" lsp-organize-imports "organise imports"))
   "Peek"
   (("d" lsp-ui-peek-find-definitions "definitions")
    ("i" lsp-ui-peek-find-implementation "implementation")
    ("r" lsp-ui-peek-find-references "references"))))

(pretty-hydra-define lsp-debugger
  (:hint nil
   :color teal
   :title (hydra-title-with-faicon "bug" "LSP Debugger"))
  ("Global"
   (("g" dap-debug "run debugger...")
    ("G" dap-debug-edit-template "debug with configuration...")
    ("i" dap-ui-inspect "inspect")
    ("K" dap-ui-inspect-thing-at-point "inspect at point..."))
   ""
   (("b" dap-ui-breakpoints "show breakpoints")
    ("l" dap-ui-sessions "list sessions")
    ("r" dap-ui-repl "REPL")
    ("x" dap-disconnect "disconnect"))
   "Breakpoints"
   (("TAB" dap-breakpoint-toggle "toggle on line")
    ("k" dap-breakpoint-condition "set condition...")
    ("h" dap-breakpoint-hit-condition "set hit condition..."))
   "Eval"
   (("e" dap-eval "string...")
    ("E" dap-eval-thing-at-point "thing at point..."))
   "Control"
   (("." dap-next "next" :exit nil)
    ("c" dap-continue "continue" :exit nil)
    ("s" dap-step-in "step in" :exit nil)
    ("u" dap-step-out "step out" :exit nil)
    ("R" dap-restart-frame "restart frame" :exit nil))))

(pretty-hydra-define annotate
  (:hint nil
   :color teal
   :title (hydra-title-with-octicon "comment" "Annotate"))
  ("Actions"
   (("c" annotate-annotate "create annotation...")
    ("n" annotate-next-annotation "next" :exit nil)
    ("p" annotate-previous-annotation "prev" :exit nil)
    ("N" annotate-previous-annotation "prev" :exit nil))
   "Views"
   (("l" annotate-show-annotation-summary "list...")
    ("<tab>" annotate-mode "toggle mode"))))



(pretty-hydra-define main-dispatcher
  (:hint nil
   :color teal
   :title (hydra-title-with-fileicon "emacs" "Overview"))
  ("Menus"
   (("$" ledger/body "ledger...")
    ("@" counsel-bookmark "bookmark...")
    ("." lsp-debugger/body "debugger...")
    ("," parens/body "parens...")
    ("a" applications/body "applications...")
    ("b" buffers/body "buffers...")
    ("c" comments/body "comments...")
    ("e" errors/body "errors...")
    ("f" files/body "files...")
    ("g" git-and-files/body "git and goto...")
    ("h" help/body "help...")
    ("j" webjump "website..."))
   ""
   (("k" kill/body "kill...")
    ("l" language-server/body  "language server...")
    ("n" narrowing/body "narrowing...")
    ("o" org/body "org...")
    ("p" project/body "project...")
    ("t" toggles/body "toggles...")
    ("v" annotate/body "annotate...")
    ("w" windows/body "windows...")
    ("y" yasnippet/body "snippets...")
    ("z" font-scale/body "font scale..."))

   "Actions"
   (("SPC" ivy-switch-buffer "buffers/recent files...")
    ("x" counsel-M-x "run command (M-x)")
    ("!" shell-command "run shell command")
    ("/" counsel-projectile-rg "search (incrementally)")
    ("S" deadgrep "search (ripgrep)")
    (":" eval-expression "evaluate lisp")
    ("'" poporg-dwim "edit comment...")
    ("C" compile "compile")
    ("d" dired "dired")
    ("D" dired-other-window "dired (other window)")
    ("r" ivy-resume "ivy-resume")
    ("s" evil-iedit-state/iedit-mode "iedit")
    ("u" universal-argument "universal argument"))

   "Buffer/Window"
   (("<tab>" alternate-buffer "alternate buffer")
    ("?" counsel-descbinds "describe key bindings")
    ("|" rotate-layout "rotate layout")
    ("i" counsel-imenu "imenu")
    ("q" delete-window "delete window"))))

;; Use general to globally bind the main dispatcher.

(use-package general
  :config
  (progn
    (general-setq general-override-states
                  '(insert emacs hybrid normal visual motion operator replace))
    (general-override-mode +1)
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'override
     "SPC" 'main-dispatcher/body)))

(provide 'config-hydras)

;;; config-hydras.el ends here
