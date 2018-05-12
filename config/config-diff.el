;;; config-diff.el --- Configure diff modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package diff-mode
  :defer t
  :preface
  (defun cb-diff-turn-off-aggressive-indent ()
    (when (fboundp 'aggressive-indent-mode)
      (aggressive-indent-mode -1)))
  :init
  (add-hook 'diff-auto-refine-mode-hook #'cb-diff-turn-off-aggressive-indent))

(use-package ediff
  :defer t
  :preface
  (progn
    (autoload 'ediff-setup-windows-plain "ediff-wind")
    (autoload 'ediff-copy-diff "ediff-util")
    (autoload 'ediff-get-region-contents "ediff-util")

    (defun cb-diff-setup-keybinds ()
      (define-key ediff-mode-map (kbd "B") #'cb-diff-ediff-copy-both-to-C))

    (defun cb-diff-ediff-copy-both-to-C ()
      (interactive)
      (let ((str
             (concat
              (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
              (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
        (ediff-copy-diff ediff-current-difference nil 'C nil str))))
  :config
  (progn
    (add-hook 'ediff-keymap-setup-hook #'cb-diff-setup-keybinds)
    (setq ediff-window-setup-function #'ediff-setup-windows-plain)))

(provide 'config-diff)

;;; config-diff.el ends here
