;;; config-search.el --- Configuration for rg, ag etc.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ag
  :straight t
  :commands ag)

(use-package rg
  :straight t
  :commands rg
  :config
  (setq rg-group-result t))

(use-package wgrep
  :straight t
  :commands (wgrep-setup)
  :init
  (add-hook 'grep-setup-hook #'wgrep-setup)
  :preface
  (progn
    (autoload 'wgrep-finish-edit "wgrep")

    (defun cb-search-wgrep-finish-edit-kill-buffer ()
      "Finish the current wgrep edit and kill the wgrep buffer."
      (interactive)
      (let ((buf (current-buffer)))
        (prog1 (wgrep-finish-edit)
          (kill-buffer buf)))))

  :config
  (progn
    (setq wgrep-enable-key (kbd "C-c C-e"))
    (setq wgrep-auto-save-buffer t)
    (define-key wgrep-mode-map [remap wgrep-finish-edit] #'cb-search-wgrep-finish-edit-kill-buffer)))

(use-package wgrep-ag
  :straight t
  :after (:any ag rg)
  :hook (rg-mode . wgrep-ag-setup))

(provide 'config-search)

;;; config-search.el ends here
