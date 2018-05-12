;;; config-csv.el --- Configuration for CSV mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode)
  :preface
  (defun config-csv--suppress-final-newline ()
    (setq-local require-final-newline nil))
  :config
  (add-hook 'csv-mode-hook #'config-csv--suppress-final-newline))

(provide 'config-csv)

;;; config-csv.el ends here
