;;; cb-highlight-thing.el --- Configuration for highlight-thing.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package highlight-thing
  :commands (highlight-thing-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-thing-mode)
  :config
  (progn
    (setq highlight-thing-what-thing 'symbol)
    (setq highlight-thing-delay-seconds 0.5)
    (setq highlight-thing-limit-to-defun nil)
    (setq highlight-thing-case-sensitive-p t)))

(provide 'cb-highlight-thing)

;;; cb-highlight-thing.el ends here
