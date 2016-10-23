;;; cb-magit.el --- Configuration for magit.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package magit
  :defer t
  :commands (magit-status) 
  :init
  (spacemacs-keys-set-leader-keys
    "gs" #'magit-status))

(use-package evil-magit
  :defer t
  :after magit) 

(provide 'cb-magit)

;;; cb-magit.el ends here
