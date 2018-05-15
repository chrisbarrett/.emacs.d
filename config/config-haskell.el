;;; config-haskell.el --- Configuration for Haskell.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-major-mode-hydra)

(autoload 'evil-define-key "evil")



(cb-major-mode-hydra-define haskell-mode
  "Insert"
  (("i" haskell-imports-insert-qualified "import")
   ("u" haskell-imports-insert-unqualified "import (unqualified)")
   ("p" haskell-pragmas-insert "language pragma"))

  "Intero"
  (("t" intero-targets "set targets"))

  "Format"
  (("f" hindent-reformat-buffer "buffer")
   ("." hindent-reformat-decl-or-fill "decl at pt")))



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
    (setq haskell-compile-cabal-build-command "stack build --ghc-options -ferror-spans")
    (setq haskell-completing-read-function #'completing-read)
    (setq haskell-interactive-popup-errors nil)

    ;; Use 4 space indentation style.

    (setq haskell-indentation-layout-offset 4)
    (setq haskell-indentation-starter-offset 2)
    (setq haskell-indentation-where-pre-offset 2)
    (setq haskell-indentation-where-post-offset 2)
    (setq haskell-indentation-left-offset 4)
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

(use-package haskell-cabal
  :after haskell-mode
  :config
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(use-package haskell-doc
  :hook (haskell-mode . haskell-doc-mode))

(use-package haskell-interactive-mode
  :hook (haskell-mode . interactive-haskell-mode))

(use-package haskell-debug
  :after haskell-mode
  :config
  (with-no-warnings
    (evilified-state-evilify-map haskell-debug-mode-map
      :mode haskell-debug-mode
      :bindings
      (kbd "n") #'haskell-debug/next
      (kbd "N") #'haskell-debug/previous
      (kbd "p") #'haskell-debug/previous
      (kbd "q") #'quit-window)))

(use-package haskell-presentation-mode
  :after haskell-mode
  :config
  (evil-define-key 'normal haskell-presentation-mode-map (kbd "q") #'quit-window))

(use-package haskell-imports
  :commands (haskell-imports-insert-unqualified
             haskell-imports-insert-qualified))

(use-package haskell-pragmas
  :commands (haskell-pragmas-insert))

(use-package intero
  :straight t
  :after haskell-mode
  :commands (intero-targets intero-goto-definition)
  :bind (:map
         intero-mode-map
         ("M-." . intero-goto-definition)
         ("M-," . pop-tag-mark))

  :preface
  (progn
    (autoload 'intero-mode-map "intero")

    (defun config-haskell--maybe-intero-mode ()
      (unless (or (derived-mode-p 'ghc-core-mode)
                  (equal (get major-mode 'mode-class) 'special))
        (intero-mode +1))))
  :init
  (add-hook 'haskell-mode-hook #'config-haskell--maybe-intero-mode)
  :config
  (progn
    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero 'haskell-hlint))

    (with-no-warnings
      (evil-define-key 'normal intero-mode-map (kbd "M-.") #'intero-goto-definition)
      (evil-define-key 'normal intero-mode-map (kbd "M-,") #'pop-tag-mark))))

(use-package hindent
  :straight t
  :hook (haskell-mode . hindent-mode)
  :config
  (setq hindent-reformat-buffer-on-save t))

(use-package stack-hoogle
  :commands (stack-hoogle-info-at-pt)
  :bind
  (:map haskell-mode-map ("C-c C-h" . stack-hoogle))
  :config
  (evil-define-key 'normal haskell-mode-map (kbd "K") #'stack-hoogle-info-at-pt))

(provide 'config-haskell)

;;; config-haskell.el ends here
