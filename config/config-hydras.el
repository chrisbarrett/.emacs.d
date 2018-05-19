;;; config-hydras.el --- Grab-bag for configuring general prefixed keys.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'all-the-icons)
(require 'buffer-cmds)
(require 'jump-cmds)
(require 'major-mode-hydra)
(require 'pretty-hydra)
(require 'spacemacs-keys)
(require 'subr-x)

(autoload 'cb/toggle-window-split "cb-toggle-window-split")
(autoload 'counsel-find-file "config-ivy")
(autoload 'counsel-recentf "config-ivy")
(autoload 'evil-window-next "evil")
(autoload 'evil-window-prev "evil")
(autoload 'evil-window-rotate-downwards "evil")
(autoload 'evil-window-split "evil")
(autoload 'evil-window-vsplit "evil")
(autoload 'generate-password/body "generate-password")
(autoload 'ivy-switch-buffer "ivy")
(autoload 'neotree-toggle "neotree")
(autoload 'org-narrow-to-subtree "org")

(use-package cb-major-mode-hydra
  :commands (cb-major-mode-hydra)
  :config
  (with-eval-after-load 'evil
    (evil-global-set-key 'motion (kbd ",") #'cb-major-mode-hydra)
    (evil-global-set-key 'normal (kbd ",") #'cb-major-mode-hydra)))


;; Define hydras as main interface for running commands.

(defmacro cb-hydra-define (name body title &rest heads-plist)
  (declare (indent defun))
  `(pretty-hydra-define ,name ,body ,heads-plist
     :docstring-prefix ,title))

(eval-and-compile
  (defun hydra-title-with-octicon (icon title)
    (concat (all-the-icons-octicon icon :face 'all-the-icons-orange :v-adjust 0.05)
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

(cb-hydra-define font-scale (:color amaranth :hint nil)
  (hydra-title-with-faicon "search-plus" "Font Scale")
  ""
  (("+" (text-scale-increase 1) "zoom in")
   ("-" (text-scale-decrease 1) "zoom out")
   ("0" (text-scale-set 0) "reset")))

(cb-hydra-define buffers (:color teal :hint nil)
  (hydra-title-with-faicon "files-o" "Buffer Commands")

  "Switch"
  (("n" next-buffer "next" :exit nil)
   ("p" previous-buffer "back" :exit nil )
   ("l" ibuffer "list")
   ("s" ivy-switch-buffer "switch"))

  "Manage"
  (("b" bury-buffer "bury" :exit nil)
   ("d" kill-this-buffer "kill")
   ("w" save-buffer "save")
   ("v" reload-file "reload")))

(cb-hydra-define windows (:color teal :hint nil)
  (hydra-title-with-faicon "clone" "Window Management")

  "Switch"
  (("w" evil-window-next "next")
   ("n" evil-window-prev "forward" :exit nil)
   ("p" evil-window-prev "back" :exit nil)
   ("r" evil-window-rotate-downwards "rotate"))

  "Split"
  (("/" evil-window-vsplit "vertical")
   ("-" evil-window-split "horizontal")
   ("=" balance-windows "rebalance"))

  "Close"
  (("d" delete-window "window")
   ("o" delete-other-windows "others")))

(cb-hydra-define files (:color teal :hint nil)
  (hydra-title-with-faicon "hdd-o" "File Commands")
  "Find"
  (("f" counsel-find-file "file")
   ("o" find-file-other-window "other window")
   ("p" find-file-at-point "at pt")
   ("h" hexl-find-file "as hex")
   ("r" counsel-recentf "recent"))

  "Save"
  (("s" save-buffer "buffer")
   ("S" save-some-buffers "many buffers")
   ("W" write-file "write copy")
   ("R" rename-file-and-buffer "rename")
   ("D" delete-current-buffer-and-file "delete"))

  "Copy"
  (("d" copy-buffer-directory "dir")
   ("y" copy-buffer-path "path")
   ("Y" copy-buffer-name "filename"))

  "Other"
  (("e" sudo-edit "edit with sudo")
   ("t" neotree-toggle "file tree")
   ("v" reload-file "reload")))

(cb-hydra-define errors (:color teal :hint nil)
  (hydra-title-with-mat-icon "error_outline" "Errors")

  "Navigation"
  (("n" flycheck-next-error "next" :color red)
   ("p" flycheck-previous-error "previous" :color red)
   ("l" config-flycheck-toggle-error-list "list errors"))

  "Actions"
  (("r" flycheck-buffer "run checks")
   ("c" flycheck-clear "clear")
   ("e" flycheck-explain-error-at-point "explain"))

  "Checkers"
  (("h" flycheck-describe-checker "describe")
   ("s" flycheck-select-checker "select")
   ("v" flycheck-verify-setup "verify")))

(cb-hydra-define help (:color teal :hint nil :help nil)
  (hydra-title-with-mat-icon "help_outline" "Help")

  "Docs"
  (("i" info "info")
   ("m" man "manpage"))

  "Describe"
  (("dc" describe-face "face")
   ("df" counsel-describe-function "function")
   ("dk" describe-key "key")
   ("dm" describe-mode "mode")
   ("dp" describe-text-properties "text-props")
   ("dv" counsel-describe-variable "variable"))

  "Find"
  (("fc" find-face-definition "face")
   ("ff" find-function "function")
   ("fl" find-library "library")
   ("fv" find-variable "variable")))

(cb-hydra-define git-and-files (:color teal :hint nil)
  (hydra-title-with-aicon "git" "Git and Goto")

  "Goto"
  (("i" jump-to-init-file "init file")
   ("m" jump-to-messages "messages")
   ("n" jump-to-nix-packages "nix packages")
   ("p" jump-to-personal-config "personal config")
   ("u" jump-to-package-usage "package usage"))

  "Git"
  (("s" magit-status "magit")
   ("d" cb-git-diff-buffer-file "blame")
   ("b" git-blame-transient-state/body "diff buffer")
   ("f" cb-git-find-file "find file")
   ("h" git-hunks-transient-state/body "navigate hunks")
   ("l" magit-log-buffer-file "log buffer")
   ("t" git-time-machine-transient-state/body "time machine"))

  "Jump to Def"
  (("g" dumb-jump-go "jump")
   ("G" dumb-jump-go-other-window "jump other window")
   ("SPC" pop-tag-mark "jump back")))

(cb-hydra-define org (:color teal :hint nil)
  (hydra-title-with-mode-icon 'org-mode "Org")
  "Actions"
  (("k" org-capture "capture...")
   ("l" org-store-link "store link...")
   ("s" org-search-view "search..."))

  "Goto"
  (("a" cb-org-goto-agenda "agenda")
   ("d" cb-org-goto-diary "diary")
   ("j" cb-org-goto-journal "journal")
   ("n" cb-org-goto-notes "notes")
   ("t" cb-org-goto-todo-list "todo list")
   ("w" cb-org-goto-work "work")
   ("v" cb-org-goto-tags-list "tags")
   ("o" cb-org-goto-headline "headline...")))

(cb-hydra-define project (:color teal :hint nil)
  (hydra-title-with-octicon "repo" "Project")

  "Actions"
  (("!" projectile-run-shell-command-in-root "shell command")
   ("c" projectile-compile-project "compile")
   ("u" projectile-run-project "run")
   ("t" cb-projectile-test-project "test"))

  "Open"
  (("f" counsel-projectile-find-file "file...")
   ("d" counsel-projectile-find-dir "directory...")
   ("b" counsel-projectile-switch-to-buffer "buffer..."))

  "Navigate"
  (("TAB" projectile-toggle-between-implementation-and-test "test/impl")
   ("<backtab>" projectile-find-implementation-or-test-other-window "test/impl (other window)")
   ("D" projectile-dired "project root (dired)"))

  "Search/Replace"
  (("/" counsel-projectile-rg "search")
   ("r" projectile-replace "replace")))

;; Application hydras

(cb-hydra-define applications (:color teal :hint nil)
  (hydra-title-with-mat-icon "apps" "Applications")

  "Productivity"
  (("c" quick-calc "quick calc")
   ("C" calc "calc")
   ("m" mu4e "mu4e")
   ("p" generate-password/body "gen password")
   ("w" world-time-list "world clock"))

  "Editing"
  (("i" select-input-method/body "input method"))

  "Emacs"
  (("?" profiler/body "profiler")
   ("s" straight/body "straight package manager"))

  "Shells"
  (("t" (ansi-term (getenv "SHELL")) "terminal")
   ("n" nix-repl-show "nix-repl")))

(cb-hydra-define profiler (:color teal :hint nil)
  (hydra-title-with-faicon "bar-chart" "Profiler")
  ""
  (("p" profiler-start "start")
   ("s" profiler-stop "stop")
   ("r" profiler-report "report")))

(cb-hydra-define select-input-method (:color teal :hint nil)
  (hydra-title-with-faicon "language" "Input Method")
  ""
  (("a" (progn (set-input-method "arabic") (message "Arabic input method activated")) "arabic")
   ("t" (progn (set-input-method "TeX") (message "TeX input method activated")) "TeX")
   ("SPC" (progn (deactivate-input-method) (message "Input method cleared")) "clear")))

(cb-hydra-define straight (:color teal :hint nil)
  (hydra-title-with-octicon "package" "Straight Package Manager")

  "Global"
  (("c" straight-prune-build "clean")
   ("n" straight-normalize-all "normalise")
   ("r" straight-rebuild-all "rebuild")
   ("P" straight-push-all "push")
   ("U" straight-pull-all "pull"))

  "Lockfile"
  (("f" straight-freeze-versions "freeze")
   ("T" straight-thaw-versions "thaw"))

  "Package"
  (("pc" straight-check-package "check")
   ("pn" straight-normalize-package "normalise")
   ("pp" straight-push-package "push")
   ("pr" straight-rebuild-package "rebuild")
   ("pu" straight-pull-package "pull")))


;; Use which-key as a fallback for stuff I haven't ported to hydras yet.

(spacemacs-keys-set-leader-keys
  "a" #'applications/body
  "b" #'buffers/body
  "e" #'errors/body
  "f" #'files/body
  "g" #'git-and-files/body
  "h" #'help/body
  "l" #'imenu-list-smart-toggle
  "m" #'major-mode-hydra
  "o" #'org/body
  "p" #'project/body
  "w" #'windows/body
  "z" #'font-scale/body)

(define-key universal-argument-map (kbd (concat "SPC u")) #'universal-argument-more)

(spacemacs-keys-set-leader-keys
  "u"   #'universal-argument
  "SPC" #'execute-extended-command
  "|"   #'cb/toggle-window-split
  ":"   #'eval-expression
  "TAB" #'alternate-buffer

  "!"   #'shell-command

  "C" #'compile

  "c r" #'comment-or-uncomment-region

  "k b" #'kill-this-buffer
  "k w" #'delete-window

  "n d" #'narrow-to-defun
  "n f" #'narrow-to-defun
  "n r" #'narrow-to-region
  "n s" #'org-narrow-to-subtree
  "n w" #'widen

  "q" #'delete-window

  "z"   #'font-scale/body)

(use-package which-key
  :straight t
  :preface
  (progn
    (autoload 'which-key-mode "which-key")
    (autoload 'which-key-add-key-based-replacements "which-key")

    (defun cb-leader-keys-set-up-which-key-buffer (&rest _)
      (when-let* ((buf (get-buffer which-key-buffer-name)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local mode-line-format nil)
            (setq-local header-line-format nil)
            (force-mode-line-update))))))

  :config
  (progn
    (setq which-key-special-keys nil)
    (setq which-key-use-C-h-commands t)
    (setq which-key-echo-keystrokes 0.02)
    (setq which-key-max-description-length 32)
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (setq which-key-idle-delay 0.02)
    (setq which-key-allow-evil-operators t)

    (advice-add 'which-key--create-buffer-and-show
                :after #'cb-leader-keys-set-up-which-key-buffer)

    ;; Strip cb prefixes from commands shown in which-key.

    (push `((nil . ,(rx bos "cb" (*? nonl) "/" (group (+? nonl))
                        (? "/body") eos))
            .
            (nil . "\\1"))
          which-key-replacement-alist)

    ;; Strip hydra body suffixes

    ;; Clean up comments entries

    (push `(("SPC c" . ,(rx (? "cb-evil-nerd-commenter/") (? "quick-") "comment-or-uncomment-" (group (+? nonl)))) . (nil . "\\1\\2"))
          which-key-replacement-alist)

    ;; Clean up errors entries

    (push `(("SPC e" . ,(rx (? "cb-") "flycheck-" (group (+? nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up goto and git

    (push `(("SPC g" . ,(rx (? "cb-") "magit-" (group (+? nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC g" . ,(rx "cb-" (group "goto-" (+? nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC g" . "time-machine-transient-state/body") . (nil . "git-time-machine"))
          which-key-replacement-alist)

    ;; Clean up help

    (push `(("SPC h d" . ,(rx bos (? "counsel-") "describe-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC h f" . ,(rx bos "find-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up navigation

    (push `(("SPC j" . ,(rx bos (? "evil-") "avy-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up kill

    (push `(("SPC k" . "kill-this-buffer") . (nil . "buffer"))
          which-key-replacement-alist)

    (push `(("SPC k" . "delete-window") . (nil . "window"))
          which-key-replacement-alist)

    (push `(("SPC k" . "counsel-yank-pop") . (nil . "kill-ring"))
          which-key-replacement-alist)

    ;; Clean up narrowing

    (push `(("SPC n" . ,(rx bos (? "org-") "narrow-to-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up org

    (push `(("SPC o" . ,(rx bos (? "cb-") (or "org-" "ledger-") (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up projectile

    (push `((nil . ,(rx bos (? "cb-") (? "counsel-") "projectile-" (group (+? nonl)) (? "-project") eos)) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `((nil . "projectile-dired") . (nil . "root (dired)"))
          which-key-replacement-alist)

    (push `((nil . "cb-neotree-find-project-root") . (nil . "root (neotree)"))
          which-key-replacement-alist)

    (push `(("SPC p" . ,(rx bos (*? nonl) "shell-command" (* nonl))) . (nil . "shell-command"))
          which-key-replacement-alist)

    (push `(("SPC p" . ,(rx bos (*? nonl) "async-shell-command" (* nonl))) . (nil . "shell-command (async)"))
          which-key-replacement-alist)

    ;; Clean up symbols

    (push `(("SPC s" . "evil-iedit-state/iedit-mode") . (nil . "iedit"))
          which-key-replacement-alist)

    ;; Clean up toggles

    (push `(("SPC t" . ,(rx bos "cb-" (? "faces/") (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up windows

    (push `(("SPC w" . ,(rx bos (? "cb-") (? "evil-") "window-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC w" . "balance-windows") . (nil . "balance"))
          which-key-replacement-alist)

    (push `(("SPC w" . "delete-window") . (nil . "delete"))
          which-key-replacement-alist)

    (push `(("SPC w" . "delete-other-windows") . (nil . "delete-others"))
          which-key-replacement-alist)

    ;; Clean up links

    (push `(("SPC x" . ,(rx bos "link-hint-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up yasnippet

    (push `(("SPC y" . ,(rx bos (? "cb-") "yas" (any "-/") (group (+? nonl)) "-snippet" eos)) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC y" . "yas-visit-snippet-file") . (nil . "visit-file"))
          which-key-replacement-alist)

    ;; Clean up transient states

    (push `((nil . ,(rx bos (group (+? nonl)) "-transient-state/body" eos)) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Fallback for any other hydras.

    (push `((nil . ,(rx bos (? "config-") (group (+? nonl)) "/body" eos)) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Add basic prefixes

    (which-key-add-key-based-replacements
      "SPC ,"   "smartparens"
      "SPC a"   "applications"
      "SPC b"   "buffers"
      "SPC c"   "comments"
      "SPC e"   "errors"
      "SPC f"   "files"
      "SPC g"   "git/goto"
      "SPC h"   "help"
      "SPC h d" "describe"
      "SPC h f" "find"
      "SPC k"   "kill"
      "SPC n"   "narrow"
      "SPC o"   "org"
      "SPC p"   "project"
      "SPC w"   "window"
      "SPC s"   "symbols"
      "SPC t"   "toggles"
      "SPC SPC" "M-x"
      "SPC m"   '("major-mode-cmd" . "Major mode commands"))

    (which-key-mode +1)))

(provide 'config-hydras)

;;; config-hydras.el ends here
