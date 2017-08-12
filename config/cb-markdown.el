;;; cb-markdown.el --- Configuration for markdown-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package fence-edit
  :after markdown-mode
  :commands (fence-edit-code-at-point)
  :config
  (progn
    (add-to-list 'fence-edit-lang-modes '("lisp" . emacs-lisp-mode))
    (define-key markdown-mode-map (kbd "C-c '") #'fence-edit-code-at-point)))

(provide 'cb-markdown)

;;; cb-markdown.el ends here
