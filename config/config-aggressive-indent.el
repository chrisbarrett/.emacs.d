;;; config-aggressive-indent.el --- Configuration for aggressive-indent-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package aggressive-indent
  :straight t
  :commands (global-aggressive-indent-mode)
  :defer 3
  :init
  (defconst aggressive-indent-excluded-modes
    '(cb-scala-sbt-file-mode
      cb-web-js-mode
      diff-auto-refine-mode
      dockerfile-mode
      fstar-mode
      graphviz-dot-mode
      haskell-mode
      idris-mode
      idris-repl-mode
      inf-ruby-mode
      makefile-gmake-mode
      makefile-mode
      nix-mode
      python-mode
      restclient-mode
      rust-mode
      scala-mode
      sql-mode
      stylus-mode
      terraform-mode
      text-mode
      toml-mode
      yaml-mode))

  :config
  (global-aggressive-indent-mode +1))

(provide 'config-aggressive-indent)

;;; config-aggressive-indent.el ends here
