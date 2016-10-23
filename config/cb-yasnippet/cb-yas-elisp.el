;;; cb-yas-elisp.el --- Helpers for Elisp snippets.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 's)

(autoload 'thing-at-point-looking-at "thingatpt")


(defun yas/bol? ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (s-matches? (rx bol (* space) (* word) (* space) eol)
              (buffer-substring (line-beginning-position) (line-end-position))))


(defun yas/find-group-for-snippet ()
  "Find the first group defined in the current file.
Fall back to the file name sans extension."
  (or
   (cadr (s-match (rx "(defgroup" (+ space) (group (+ (not
                                                       space))))
                  (buffer-string)))
   (cadr (s-match (rx ":group" (+ space) "'" (group (+ (any "-" alnum))))
                  (buffer-string)))
   (file-name-sans-extension (file-name-nondirectory buffer-file-name))))


(defun yas/autoload-file-for-function (sym)
  (if-let (file (symbol-file (if (stringp sym) (intern sym) sym)))
      (file-name-sans-extension (file-name-nondirectory file))
    ""))

(defun yas/at-line-above-decl? ()
  (save-excursion
    (forward-line)
    (back-to-indentation)
    (thing-at-point-looking-at (rx (* space) "("
                                   (or "cl-defun" "defun" "defvar" "defconst"
                                       "define-minor-mode"
                                       "define-globalized-minor-mode"
                                       "define-derived-mode")))))


(provide 'cb-yas-elisp)

;;; cb-yas-elisp.el ends here
