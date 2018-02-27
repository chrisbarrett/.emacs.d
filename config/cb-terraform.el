;;; cb-terraform.el --- Config for terraform.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package terraform-mode
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode))

(provide 'cb-terraform)

;;; cb-terraform.el ends here
