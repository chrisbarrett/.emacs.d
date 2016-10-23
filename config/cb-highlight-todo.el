;;; cb-highlight-todo.el --- Configuration for todo highlighting.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package hl-todo
  :defer t
  :commands (hl-todo-mode)

  :preface
  (defun cb-highlight-todo--enable-unless-org-buffer ()
    (unless (derived-mode-p 'org-mode)
      (hl-todo-mode)))

  :init
  (progn
    (add-hook 'prog-mode-hook #'hl-todo-mode)
    (add-hook 'text-mode-hook #'cb-highlight-todo--enable-unless-org-buffer)))

(provide 'cb-highlight-todo)

;;; cb-highlight-todo.el ends here
