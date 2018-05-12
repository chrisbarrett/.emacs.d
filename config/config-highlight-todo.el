;;; config-highlight-todo.el --- Configuration for todo highlighting.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)

(use-package hl-todo
  :straight t
  :defer t
  :commands (hl-todo-mode)

  :preface
  (defun cb-highlight-todo--enable-unless-org-buffer ()
    (unless (derived-mode-p 'org-mode)
      (hl-todo-mode)))

  :init
  (progn
    (setq hl-todo-keyword-faces
          (--map (cons it 'hl-todo)
                 '("TODO"
                   "NEXT"
                   "HACK"
                   "FIXME"
                   "KLUDGE"
                   "NOTE")))
    (add-hook 'prog-mode-hook #'hl-todo-mode)
    (add-hook 'text-mode-hook #'cb-highlight-todo--enable-unless-org-buffer)))

(provide 'config-highlight-todo)

;;; config-highlight-todo.el ends here
