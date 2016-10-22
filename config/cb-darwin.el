;;; cb-darwin.el --- OSX-specific configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'copy-region-as-kill)
(global-set-key (kbd "s-n") 'new-frame)
(global-set-key (kbd "s-w") 'delete-frame)

(use-package exec-path-from-shell
  :if window-system
  :config
  (exec-path-from-shell-initialize)
  :functions
  (exec-path-from-shell-initialize))

(use-package osx-trash
  :config
  (osx-trash-setup)
  :functions
  (osx-trash-setup))


(provide 'cb-darwin)

;;; cb-darwin.el ends here
