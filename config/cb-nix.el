;;; cb-nix.el --- Configuration for NixOS.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(autoload 'evil-set-initial-state "evil-core")

(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.nix.in\\'" . nix-mode))
  :init
  ;; Emacs.app on macOS doesn't inherit from a shell, so setting NIX_REMOTE is
  ;; needed to get nix to work.
  (when (equal system-type 'darwin)
    (setenv "NIX_REMOTE" "daemon")))

(use-package nix-repl
  :commands nix-repl-show
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
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-nixos-options)))

(provide 'cb-nix)

;;; cb-nix.el ends here
