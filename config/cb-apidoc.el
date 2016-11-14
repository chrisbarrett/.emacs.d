;;; cb-apidoc.el --- Configuration for Apidoc.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package apidoc
  :preface
  (autoload 'yaml-mode "yaml-mode")
  :init
  (progn
    (add-to-list 'auto-mode-alist (cons "api\\.json$" #'cb-web-json-mode))
    (add-to-list 'auto-mode-alist (cons "\\.apidoc$" #'yaml-mode))))

(use-package apidoc-checker
  :load-path "~/Projects/apidoc-checker/elisp/"
  :preface
  (autoload 'flycheck-add-mode "flycheck")
  :config
  (flycheck-add-mode 'apidoc 'cb-web-json-mode))

(provide 'cb-apidoc)

;;; cb-apidoc.el ends here
