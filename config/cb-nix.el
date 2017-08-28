;;; cb-nix.el --- Configuration for NixOS.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.nix.in\\'" . nix-mode)))

(use-package company-nixos-options
  :after nix-mode
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-nixos-options)))

(provide 'cb-nix)

;;; cb-nix.el ends here
