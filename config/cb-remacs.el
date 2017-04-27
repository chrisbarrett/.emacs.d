;;; cb-remacs.el --- Remacs-specific customisations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(autoload 'f-join "f")

(defconst cb-remacs-directory (concat (getenv "HOME") "/Code/remacs"))

(use-package with-editor
  :defer t
  :init
  (defvar with-editor-emacsclient-executable
    (f-join cb-remacs-directory "lib-src/remacsclient")))

(provide 'cb-remacs)

;;; cb-remacs.el ends here
