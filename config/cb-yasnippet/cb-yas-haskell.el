;;; cb-yas-haskell.el --- Helpers for haskell snippets  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)

(defvar yas-text)

(defun cb-yas-haskell-ctor-name (&optional text)
  (car (s-split (rx space) (or text yas-text))))

(provide 'cb-yas-haskell)

;;; cb-yas-haskell.el ends here
