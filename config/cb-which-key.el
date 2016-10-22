;;; cb-which-key.el --- Configuration for which-key.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package which-key
  :config
  (which-key-mode +1))

(provide 'cb-which-key)

;;; cb-which-key.el ends here
