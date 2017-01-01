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
  :preface
  (defun cb-autoinsert--maybe-enter-insert-state (fn &rest args)
    (let ((new-file? (string-empty-p (buffer-string))))
      (apply fn args)
      (when new-file?
        (evil-insert-state))))
  :config
  (progn
    (setq auto-insert-query nil)
    (auto-insert-mode +1)
    (advice-add 'auto-insert :around #'cb-autoinsert--maybe-enter-insert-state)))

(provide 'cb-autoinsert)

;;; cb-autoinsert.el ends here
