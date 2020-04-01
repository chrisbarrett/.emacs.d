;;; config-nix.el --- Configuration for Nix package manager.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(global-set-key (kbd "s-v") #'clipboard-yank)



(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.nix.in\\'" . nix-mode))
  :config
  (general-setq nix-indent-function #'nix-indent-line))

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

(use-package nix-repl
  :commands nix-repl-show
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Nix-REPL*" eos)
                 (display-buffer-reuse-window
                  display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (slot            . 1)
                 (window-height   . 0.4))))

(provide 'config-nix)

;;; config-nix.el ends here
