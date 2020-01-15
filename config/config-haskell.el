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
   (("f" hindent-reformat-buffer "buffer")
    ("." hindent-reformat-decl-or-fill "decl at pt"))))

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

    (add-hook 'haskell-mode-hook #'config-haskell--set-indentation-step)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*intero:")
                   (display-buffer-reuse-window
                    display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (slot            . 1)
                   (window-width    . 0.5)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*stack hoogle*" eos)
                   (display-buffer-reuse-window
                    display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (slot            . 1)
                   (window-height   . 0.5)))))

(use-package haskell-imports
  :commands (haskell-imports-insert-unqualified
             haskell-imports-insert-qualified))

(use-package haskell-pragmas
  :commands (haskell-pragmas-insert))

(use-package dante
  :straight t
  :preface
  (defun config-haskell--configure-dante ()
    (setq-local flymake-no-changes-timeout nil)
    (setq-local flymake-start-syntax-check-on-newline nil)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))

  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . dante-mode))
  :commands (dante-mode)
  :config
  (add-hook 'dante-mode-hook #'config-haskell--configure-dante))

(use-package hindent
  :straight t
  :defer t
  :hook (haskell-mode . hindent-mode)
  :config
  (setq hindent-reformat-buffer-on-save t))

(use-package stack-hoogle
  :commands (stack-hoogle-info-at-pt)
  :general (:keymaps 'haskell-mode-map "C-c C-h" #'stack-hoogle)
  :general (:states 'normal :keymaps 'haskell-mode-map "K" #'stack-hoogle-info-at-pt))

(provide 'config-haskell)

;;; config-haskell.el ends here
