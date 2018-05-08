;;; cb-undo-tree.el --- Configure undo-tree.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package undo-tree
  :straight t
  :demand t
  :commands (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (global-undo-tree-mode)))

(provide 'cb-undo-tree)

;;; cb-undo-tree.el ends here
