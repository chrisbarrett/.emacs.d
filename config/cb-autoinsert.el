;;; cb-autoinsert.el --- General configuration for autoinsert. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(use-package autoinsert
  :init
  (setq auto-insert-alist nil)
  :config
  (progn
    (setq auto-insert-query nil)
    (auto-insert-mode +1)))

(provide 'cb-autoinsert)

;;; cb-autoinsert.el ends here
