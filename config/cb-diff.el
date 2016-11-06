;;; cb-diff.el --- Configure diff modes  -*- lexical-binding: t; -*-

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
  :functions
  :preface
  (autoload 'ediff-setup-windows-plain "ediff-wind")
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(provide 'cb-diff)

;;; cb-diff.el ends here
