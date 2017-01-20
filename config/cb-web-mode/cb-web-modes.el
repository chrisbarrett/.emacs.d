;;; cb-web-modes.el --- Major modes derived from web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'web-mode)

;;;###autoload
(define-derived-mode cb-web-js-mode web-mode "JS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode cb-web-typescript-mode web-mode "TS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode cb-web-json-mode web-mode "JSON"
  "Derived mode for editing JSON files."
  (setq-local web-mode-content-type "json"))

;;;###autoload
(define-derived-mode cb-web-html-mode web-mode "HTML"
  "Derived mode for editing HTML files."
  (setq-local web-mode-content-type "html"))

;;;###autoload
(define-derived-mode cb-web-css-mode web-mode "CSS"
  "Derived mode for editing CSS files."
  (setq-local web-mode-content-type "css"))

(provide 'cb-web-modes)

;;; cb-web-modes.el ends here
