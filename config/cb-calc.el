;;; cb-calc.el --- Configuration for calc  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package calc
  :commands (calc quick-calc)
  :init
  (spacemacs-keys-set-leader-keys
    "a c" #'quick-calc
    "a C" #'calc)
  :config
  (define-key calc-mode-map (kbd "SPC") spacemacs-keys-default-map))


(provide 'cb-calc)

;;; cb-calc.el ends here
