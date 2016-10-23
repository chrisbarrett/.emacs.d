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
  (global-aggressive-indent-mode +1))

(provide 'cb-aggressive-indent)

;;; cb-aggressive-indent.el ends here
