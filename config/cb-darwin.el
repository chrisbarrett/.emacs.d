;;; cb-darwin.el --- OSX-specific configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-emacs)

(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'copy-region-as-kill)
(global-set-key (kbd "s-w") 'delete-frame)
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-q"))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . 'nil))


;; Graphical applications in macOS inherit their process environment from
;; launchd, not from a shell process which loads a profile.

(use-package exec-path-from-shell
  :straight t
  :if window-system
  :functions (exec-path-from-shell-initialize)
  :init
  (defconst exec-path-from-shell-variables
    '("PATH"
      "MANPATH"
      "GITHUB_TOKEN"
      "NIX_REMOTE"
      "NIX_USER_PROFILE_DIR"
      "NIX_PROFILES"
      "NIX_SSL_CERT_FILE"
      "NIX_PATH"
      "SSH_AGENT_PID"
      "GPG_TTY"
      "TEXINPUTS"
      "RUST_SRC_PATH"))
  :config
  (exec-path-from-shell-initialize))

(use-package osx-trash
  :straight t
  :preface
  (autoload 'osx-trash-setup "osx-trash")
  :config
  (osx-trash-setup))

(use-package dash-at-point
  :straight t
  :bind ("<f8>" . dash-at-point))

(use-package time
  :commands (display-time-mode)
  :init
  (add-hook 'after-init-hook #'display-time-mode)
  :config
  (setq display-time-default-load-average nil))

(provide 'cb-darwin)

;;; cb-darwin.el ends here
