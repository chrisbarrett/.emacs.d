;;; cb-ag.el --- Configuration for ag and related utils.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ag
  :commands ag)

(use-package wgrep
  :defer t
  :preface
  (progn
    (autoload 'wgrep-finish-edit "wgrep")

    (defun cb-ag-wgrep-finish-edit-kill-buffer ()
      "Finish the current wgrep edit and kill the wgrep buffer."
      (interactive)
      (let ((buf (current-buffer)))
        (prog1 (wgrep-finish-edit)
          (kill-buffer buf)))))

  :config
  (progn
    (setq wgrep-auto-save-buffer t)
    (define-key wgrep-mode-map [remap wgrep-finish-edit] #'cb-ag-wgrep-finish-edit-kill-buffer)))

(use-package wgrep-ag
  :after ag)

(provide 'cb-ag)

;;; cb-ag.el ends here
