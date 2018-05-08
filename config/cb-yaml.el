;;; cb-yaml.el --- Configuration for YAML mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package yaml-mode
  :straight t
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))

(provide 'cb-yaml)

;;; cb-yaml.el ends here
