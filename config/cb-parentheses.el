;;; cb-parentheses.el --- Configuration for parens.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package paren-face
  :demand t
  :commands (global-paren-face-mode)
  :config
  (progn
    (add-to-list 'paren-face-modes 'scala-mode)
    (setq paren-face-regexp "[{}()]")
    (global-paren-face-mode +1)))


(provide 'cb-parentheses)

;;; cb-parentheses.el ends here
