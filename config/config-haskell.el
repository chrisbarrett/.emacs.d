;;; config-haskell.el --- Configuration for Haskell.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)

(major-mode-hydra-define haskell-mode nil
  ("Insert"
   (("i" haskell-imports-insert-qualified "import")
    ("u" haskell-imports-insert-unqualified "import (unqualified)")
    ("p" haskell-pragmas-insert "language pragma"))))

;; `haskell-mode' is the main package implementing haskell language support.

(use-package haskell-mode
  :straight t
  :mode
  (("\\.[gh]s\\'" . haskell-mode)
   ("\\.l[gh]s\\'" . literate-haskell-mode)
   ("\\.hsc\\'" . haskell-mode)
   ("\\.cabal". haskell-cabal-mode))

  :interpreter
  (("runghc" . haskell-mode)
   ("runhaskell" . haskell-mode))

  :hook (haskell-mode . interactive-haskell-mode)

  :general
  (:states 'normal :keymaps 'haskell-presentation-mode-map "q" #'quit-window)
  (:keymaps 'haskell-cabal-mode-map "C-c C-c" #'haskell-compile)
  (:states 'motion :keymaps 'haskell-debug-mode-map
   "n" #'haskell-debug/next
   "N" #'haskell-debug/previous
   "p" #'haskell-debug/previous
   "q" #'quit-window)

  :preface
  (defun config-haskell--wrap-commands-with-nix-shell (argv)
    (if (locate-dominating-file default-directory "shell.nix")
        (list "nix-shell" "-I" "." "--command" (string-join (-list argv) " "))
      argv))

  :init
  (progn
    (add-to-list 'completion-ignored-extensions ".hi")
    (add-to-list 'completion-ignored-extensions ".gm"))

  :config
  (general-setq haskell-completing-read-function #'completing-read
                haskell-interactive-popup-errors nil
                haskell-process-wrapper-function #'config-haskell--wrap-commands-with-nix-shell))

;; `haskell-imports' provides helper commands for inserting import statements.

(use-package haskell-imports
  :commands (haskell-imports-insert-unqualified
             haskell-imports-insert-qualified))

;; `haskell-pragmas' provides helper commands for inserting language extension
;; pragmas.

(use-package haskell-pragmas
  :commands (haskell-pragmas-insert))

;; `dante' extends haskell-mode to provide extra IDE-like functionality.

(use-package dante
  :straight t
  :preface
  (defun config-haskell--configure-dante ()
    (setq-local flymake-no-changes-timeout nil)
    (setq-local flymake-start-syntax-check-on-newline nil)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (setq-local evil-lookup-func (lambda () (call-interactively #' dante-info))))

  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . dante-mode))
  :commands (dante-mode)
  :general
  (:states '(insert normal) :keymaps 'dante-mode-map
   "C-c C-SPC" #'dante-eval-block
   "M-." #'xref-find-definitions)
  :config
  (progn
    (add-hook 'dante-mode-hook #'config-haskell--configure-dante)
    (general-setq
     dante-tap-type-time 0.1
     dante-load-flags
     '("+c"
       "-Wall"
       "-fdefer-typed-holes"
       "-fdiagnostics-color=never"
       "-fno-diagnostics-show-caret"
       "-Wwarn=missing-home-modules"
       "-ferror-spans"))

    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))))

;; `reformatter' provides a way to declare formatting commands. Use it to
;; declare a binding to the ormolu Haskell src formatter.
;;
;; There is an existing ormolu package, but it invokes with nix-shell. Instead,
;; I want to use the ormolu executable on the path. This will either be:
;;
;; 1. the project's ormolu, as injected by direnv
;; 2. the fallback installation in my nix user-profile.
(use-package reformatter
  :after (haskell)
  :straight t
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :config
  (reformatter-define ormolu-format
    :program "ormolu"
    :args '("/dev/stdin")))

(provide 'config-haskell)

;;; config-haskell.el ends here
