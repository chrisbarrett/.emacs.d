;;; cb-info.el --- Configuration for info packages.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chrisb@Chris-MacBook-Pro.local>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package info+
  :defer t
  :preface
  (defvar Info-fontify-angle-bracketed-flag)
  :init
  (progn
    (with-eval-after-load 'info
      (require 'info+))
    (setq Info-fontify-angle-bracketed-flag nil)))

(provide 'cb-info)

;;; cb-info.el ends here
