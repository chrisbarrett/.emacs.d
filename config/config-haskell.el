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
    ("p" haskell-pragmas-insert "language pragma"))
   "Format"
   (("r" ormolu-format-buffer "format buffer"))))

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

  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode))

  :general
  (:states 'normal :keymaps 'haskell-presentation-mode-map "q" #'quit-window)
  (:keymaps 'haskell-cabal-mode-map "C-c C-c" #'haskell-compile)
  (:states 'motion :keymaps 'haskell-debug-mode-map
   "n" #'haskell-debug/next
   "N" #'haskell-debug/previous
   "p" #'haskell-debug/previous
   "q" #'quit-window)

  :preface
  (progn
    (defun config-haskell--wrap-commands-with-nix-shell (argv)
      (if (locate-dominating-file default-directory "shell.nix")
          (list "nix-shell" "-I" "." "--command" (string-join (-list argv) " "))
        argv))

    (defun config-haskell--set-indentation-step ()
      (when (boundp 'evil-shift-width)
        (setq evil-shift-width 4))
      (setq tab-width 4)))

  :init
  (progn
    (add-to-list 'completion-ignored-extensions ".hi")
    (add-to-list 'completion-ignored-extensions ".gm"))

  :config
  (progn
    (general-setq
     haskell-completing-read-function #'completing-read
     haskell-interactive-popup-errors nil
     haskell-process-wrapper-function #'config-haskell--wrap-commands-with-nix-shell

     ;; Use 4 space indentation style.

     haskell-indentation-layout-offset 4
     haskell-indentation-starter-offset 2
     haskell-indentation-where-pre-offset 2
     haskell-indentation-where-post-offset 2
     haskell-indentation-left-offset 4)

    (add-hook 'haskell-mode-hook #'config-haskell--set-indentation-step)))

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
   "C-c C-SPC" #'dante-eval-block)
  :config
  (add-hook 'dante-mode-hook #'config-haskell--configure-dante))

;; `reformatter' provides a way to declare formatting commands. Use it to
;; declare a binding to the ormolu Haskell src formatter.

(use-package reformatter
  :after (haskell)
  ;; FIXME: ormolu seems to be broken on nixpkgs right now.
  :disabled t
  :straight t
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :config
  (with-no-warnings
    (reformatter-define ormolu-format
                        :program "nix-shell"
                        :args '("-I" "." "--command" "ormolu /dev/stdin"))))

(provide 'config-haskell)

;;; config-haskell.el ends here
