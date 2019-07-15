;;; config-haskell.el --- Configuration for Haskell.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)

(major-mode-hydra-bind haskell-mode "Insert"
  ("i" haskell-imports-insert-qualified "import")
  ("u" haskell-imports-insert-unqualified "import (unqualified)")
  ("p" haskell-pragmas-insert "language pragma"))

(major-mode-hydra-bind haskell-mode "Intero"
  ("t" intero-targets "set targets"))

(major-mode-hydra-bind haskell-mode "Format"
  ("f" hindent-reformat-buffer "buffer")
  ("." hindent-reformat-decl-or-fill "decl at pt"))



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
  :hook (haskell-mode . haskell-doc-mode)

  :general (:states 'normal :keymaps 'haskell-presentation-mode-map "q" #'quit-window)
  :general (:keymaps haskell-cabal-mode-map "C-c C-c" #'haskell-compile)

  :general (:states 'motion :keymaps 'haskell-debug-mode-map
            "n" #'haskell-debug/next
            "N" #'haskell-debug/previous
            "p" #'haskell-debug/previous
            "q" #'quit-window)

  :init
  (progn
    (add-to-list 'completion-ignored-extensions ".hi")
    (add-to-list 'completion-ignored-extensions ".gm"))

  :preface
  (defun config-haskell--set-indentation-step ()
    (with-no-warnings (setq evil-shift-width 4))
    (setq tab-width 4))

  :config
  (progn
    (general-setq
     haskell-compile-cabal-build-command "stack build --ghc-options -ferror-spans"
     haskell-completing-read-function #'completing-read
     haskell-interactive-popup-errors nil

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
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width    . 0.5)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*stack hoogle*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.5)))))

(use-package haskell-imports
  :commands (haskell-imports-insert-unqualified
             haskell-imports-insert-qualified))

(use-package haskell-pragmas
  :commands (haskell-pragmas-insert))

(use-package intero
  :straight t
  :defer t
  :after haskell-mode
  :general
  (:states '(normal insert emacs) :keymaps 'intero-mode-map
   "M-." #'intero-goto-definition
   "M-," #'pop-tag-mark)
  :config
  (progn
    (intero-global-mode +1)

    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero 'haskell-hlint))))

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
