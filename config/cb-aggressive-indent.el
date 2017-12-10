;;; cb-aggressive-indent.el --- Configuration for aggressive-indent-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package aggressive-indent
  :commands (global-aggressive-indent-mode)
  :defer 1
  :config
  (progn
    (add-to-list 'aggressive-indent-excluded-modes 'cb-scala-sbt-file-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'cb-web-js-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'diff-auto-refine-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'fstar-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'graphviz-dot-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'idris-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'idris-repl-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'restclient-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'rust-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'stylus-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'toml-mode)

    (global-aggressive-indent-mode +1)))

(provide 'cb-aggressive-indent)

;;; cb-aggressive-indent.el ends here
