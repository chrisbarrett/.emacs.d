;;; cb-elisp-sp.el --- Smartparens utils for Elisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'seq)

(autoload 'sp-get-pair "smartparens")
(autoload 'sp-in-code-p "smartparens")
(autoload 'sp-in-string-p "smartparens")

(defun cb-elisp-sp--just-inserted-double-quotes? (id action ctx)
  (and (sp-in-string-p id action ctx)
       (s-matches? (rx (not (any "\\")) "\"" eol)
                   (buffer-substring (line-beginning-position) (point)))))

;;;###autoload
(defun cb-elisp-sp-just-one-space (id action ctx)
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (cb-elisp-sp--just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is a quoted form
    ;; 2. this is the first position of another list
    ;; 3. this form begins a new line.
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "," "`" "'" "@" "#" "~" "(" "[" "{")
                       ;; HACK: nREPL prompt
                       (and (any alnum "." "/" "-") ">" (* space)))
                   eol)
               (buffer-substring (line-beginning-position) (point)))
        (just-one-space)))
    ;; Insert space after separator, unless
    ;; 1. this form is at the end of another list.
    ;; 2. this form is at the end of the line.
    (save-excursion
      (search-forward (sp-get-pair id :close))
      (unless (seq-contains '(")" "]" "}") (char-to-string (char-after)))
        (just-one-space)))))

(provide 'cb-elisp-sp)

;;; cb-elisp-sp.el ends here
