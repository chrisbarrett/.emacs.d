;;; cb-yas-js.el --- Snippet utils for JS.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)

(defun cb-yas-js--ctor-body (s)
  (when s
    (->> (s-split (rx (or "," ".")) s)
         (-map #'s-trim)
         (-remove #'s-blank?)
         (--map (format "this.%s = %s;" it it))
         (s-join "\n"))))

(provide 'cb-yas-js)

;;; cb-yas-js.el ends here
