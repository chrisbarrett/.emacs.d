;;; cb-autoinsert.el --- General configuration for autoinsert. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package autoinsert
  :init
  (defvar auto-insert-alist nil)
  :config
  (progn
    (setq auto-insert-query nil)
    (auto-insert-mode +1)))

(provide 'cb-autoinsert)

;;; cb-autoinsert.el ends here
