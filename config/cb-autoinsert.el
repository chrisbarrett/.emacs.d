;;; cb-autoinsert.el --- General configuration for autoinsert. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(autoload 'evil-insert-state "evil-states")
(autoload 'string-empty-p "subr-x")

(use-package autoinsert
  :init
  (defvar auto-insert-alist nil)
  :init
  (auto-insert-mode +1)
  :config
  (setq auto-insert-query nil))

(provide 'cb-autoinsert)

;;; cb-autoinsert.el ends here
