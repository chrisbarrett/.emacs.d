;;; web-mode-submodes.el --- Major modes derived from web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'seq)

;; Placate byte-compiler

(eval-when-compile
  (require 'web-mode))
(autoload 'web-mode "web-mode")



;;;###autoload
(define-derived-mode web-js-base-mode web-mode "JS"
  "Derived mode for editing JavaScript files."
  (when (seq-contains '("" "html") web-mode-content-type)
    (setq-local web-mode-content-type "javascript")))

;;;###autoload
(define-derived-mode web-js-mode web-js-base-mode "JS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode web-js-snap-mode web-js-mode "JS"
  "Derived mode for editing Jest snapshot files.")

;;;###autoload
(define-derived-mode web-ts-mode web-js-base-mode "TS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode web-json-mode web-mode "JSON"
  "Derived mode for editing JSON files."
  (setq-local web-mode-content-type "json"))

;;;###autoload
(define-derived-mode web-html-mode web-mode "HTML"
  "Derived mode for editing HTML files."
  (setq-local web-mode-content-type "html"))

;;;###autoload
(define-derived-mode web-css-mode web-mode "CSS"
  "Derived mode for editing CSS files."
  (setq-local web-mode-content-type "css"))

;;;###autoload
(define-derived-mode web-mustache-mode web-mode "Mustache"
  "Derived mode for editing mustache files.")

;;;###autoload
(define-derived-mode avro-mode web-mode "Avro"
  "Derived mode for editing Avro schema files."
  (setq-local web-mode-content-type "json"))

(provide 'web-mode-submodes)

;;; web-mode-submodes.el ends here
