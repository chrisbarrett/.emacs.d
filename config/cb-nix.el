;;; cb-nix.el --- Configuration for NixOS.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.nix.in\\'" . nix-mode)))


(provide 'cb-nix)

;;; cb-nix.el ends here
