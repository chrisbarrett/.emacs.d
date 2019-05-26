;;; config-hydras.el --- Main hydra definitions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'buffer-cmds)
(require 'jump-cmds)
(require 'window-cmds)

(autoload 'cb/toggle-window-split "cb-toggle-window-split")
(autoload 'counsel-find-file "config-ivy")
(autoload 'counsel-recentf "config-ivy")
(autoload 'evil-window-next "evil")
(autoload 'evil-window-prev "evil")
(autoload 'evil-window-rotate-downwards "evil")
(autoload 'generate-password/body "generate-password")
(autoload 'ivy-switch-buffer "ivy")
(autoload 'neotree-toggle "neotree")
(autoload 'org-narrow-to-subtree "org")

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

(use-package all-the-icons :straight t)

(use-package pretty-hydra
  :straight t
  :demand t
  :preface
  (defun config-hydras--add-quit-bindings (result)
    (append '(("q" nil :exit t)
              ("<escape>" nil :exit t))
            result))
  :config
  (advice-add #'pretty-hydra--get-heads :filter-return #'config-hydras--add-quit-bindings))

(use-package hydra-posframe
  :unless (getenv "TERM_PROGRAM")
  :straight (:host github :repo "Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-enable)
  :config
  (setq hydra-posframe-parameters '((alpha 100 100))
        hydra-posframe-border-width 20))


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
    ("t" treemacs "file tree")
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
    ("s" #'org-narrow-to-subtree "org subtree"))
   "Actions"
   (("w" #'widen "widen"))))

(pretty-hydra-define org
  (:hint nil
   :color teal
   :title (hydra-title-with-mode-icon 'org-mode "Org"))
  ("Actions"
   (("k" org-capture "capture...")
    ("l" org-store-link "store link...")
    ("s" org-search-view "search..."))

   "Goto"
   (("$" (find-file ledger-master-file) "ledger")
    ("a" config-org-agenda/body "agenda")
    ("d" cb-org-goto-diary "diary")
    ("j" cb-org-goto-journal "journal")
    ("n" cb-org-goto-notes "notes"))
   ""
   (("t" cb-org-goto-todo-list "todo list")
    ("w" cb-org-goto-work "work")
    ("v" cb-org-goto-tags-list "tags")
    ("o" cb-org-goto-headline "headline..."))))

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
   (("c" hide/show-comments-toggle "comments"))))

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
    ("f" config-git-find-file "find file..."))
   ""
   (("h" git-hunks/body "navigate hunks")
    ("l" magit-log-buffer-file "log buffer")
    ("t" git-time-machine/body "time machine"))

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

(pretty-hydra-define git-blame
  (:foreign-keys run
   :pre (unless (bound-and-true-p magit-blame-mode)
          (call-interactively 'magit-blame))
   :post
   (when (bound-and-true-p magit-blame-mode)
     (magit-blame-quit))
   :title (hydra-title-with-aicon "git" "Git Blame"))
  ("Display"
   (("b" magit-blame "rev added")
    ("d" magit-blame-removal "rev removed")
    ("r" magit-blame-reverse "last rev where exists"))))

(pretty-hydra-define git-hunks
  (:foreign-keys run
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

   "Editing"
   (("i" toggle-input-method "toggle input method"))

   "Emacs"
   (("r" profiler/body "profiler...")
    ("s" straight/body "straight package manager...")
    ("v" prodigy "services..."))

   "Shells"
   (("t" cb-eshell-at-dir "terminal (eshell)")
    ("n" nix-repl-show "nix-repl"))))

(pretty-hydra-define profiler
  (:hint nil
   :color teal
   :title (hydra-title-with-faicon "bar-chart" "Profiler"))
  (""
   (("p" profiler-start "start...")
    ("s" profiler-stop "stop")
    ("r" profiler-report "report"))))

(pretty-hydra-define straight
  (:hint nil
   :color teal
   :title (hydra-title-with-octicon "package" "Straight Package Manager"))
  ("Global"
   (("c" straight-prune-build "clean")
    ("n" straight-normalize-all "normalise")
    ("r" straight-rebuild-all "rebuild")
    ("P" straight-push-all "push")
    ("U" straight-pull-all "pull"))

   "Lockfile"
   (("f" straight-freeze-versions "freeze")
    ("T" straight-thaw-versions "thaw"))

   "Package"
   (("pc" straight-check-package "check...")
    ("pn" straight-normalize-package "normalise...")
    ("pp" straight-push-package "push...")
    ("pr" straight-rebuild-package "rebuild...")
    ("pu" straight-pull-package "pull..."))))



(pretty-hydra-define main-dispatcher
  (:hint nil
   :color teal
   :title (hydra-title-with-fileicon "emacs" "Overview"))
  ("Menus"
   (("," parens/body "parens...")
    ("a" applications/body "applications...")
    ("b" buffers/body "buffers...")
    ("c" comments/body "comments...")
    ("e" errors/body "errors...")
    ("f" files/body "files...")
    ("g" git-and-files/body "git and goto...")
    ("h" help/body "help...")
    ("k" kill/body "kill..."))
   ""
   (("n" narrowing/body "narrowing...")
    ("o" org/body "org...")
    ("p" project/body "project...")
    ("t" toggles/body "toggles...")
    ("w" windows/body "windows...")
    ("y" yasnippet/body "snippets...")
    ("z" font-scale/body "font scale..."))

   "Actions"
   (("SPC" counsel-M-x "run command (M-x)")
    ("!" shell-command "run shell command")
    ("/" counsel-projectile-rg "search project")
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
    ("|" cb/toggle-window-split "toggle window split")
    ("i" counsel-imenu "imenu")
    ("l" imenu-list-smart-toggle "imenu list")
    ("q" delete-window "delete window"))))

;; Use general to globally bind the main dispatcher.

(use-package general
  :straight t
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
