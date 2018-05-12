;;; yas-funcs.el --- Functions for yasnippets.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;;; Commentary:

;;; Code:

(require 's)


;;; JS

(defun yas-funcs-js-module-name-for-binding (yas-text)
  (pcase yas-text
    ('nil      "")
    (""        "")
    ("Promise" "bluebird")
    ("assert"  "power-assert")
    ("_"       "lodash")

    ((guard (s-contains? "{" yas-text))
     "MODULE")
    (s
     (s-downcase (s-dashed-words s)))))


(provide 'yas-funcs)

;;; yas-funcs.el ends here
