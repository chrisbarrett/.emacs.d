;;; cb-nix.el --- Configuration for NixOS.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'company nil t)
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'f-ext? "f")
(autoload 'evil-set-initial-state "evil-core")

(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.nix.in\\'" . nix-mode)))

(use-package compile
  :defer t
  :config
  (progn
    (defconst cb-nix--compile-error-regexp
      (rx bol "error:" (+? nonl) symbol-start "at "
          (group (+? nonl))
          ":"
          (group (+? digit))
          ":"
          (group (+? digit))
          eol))

    (add-to-list 'compilation-error-regexp-alist 'nix-env)
    (add-to-list 'compilation-error-regexp-alist-alist
                 `(nix-env . (,cb-nix--compile-error-regexp 1 2 3)))))

(use-package nix-shell
  :commands nix-shell)

(use-package nix-company
  :after nix-mode
  :config
  (add-to-list 'company-backends 'company-nix))

(use-package nix-repl
  :commands nix-repl-show
  :init
  (spacemacs-keys-set-leader-keys "an" #'nix-repl-show)
  :config
  (progn
    (evil-set-initial-state 'nix-repl-mode 'insert)
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Nix-REPL*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.4)))))

(use-package company-nixos-options
  :after nix-mode
  :config
  (progn
    (add-to-list 'company-backends 'company-nixos-options)

    ;; KLUDGE: Redefine function that throws error.
    (defun company-nixos--in-nix-context-p ()
      (or (derived-mode-p 'nix-mode)
          (when (buffer-file-name)
            (f-ext? (buffer-file-name) "nix"))))))

(provide 'cb-nix)

;;; cb-nix.el ends here
