;;; cb-js-refactor-commands.el --- Commands for refactoring Javascript  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'seq)
(require 'smartparens)
(require 'subr-x)
(require 'web-mode)

(defun cb-js-refactor-commands--binding-keyword-at-pt ()
  (seq-contains '(const var let) (symbol-at-point)))

(defun cb-js-refactor-commands--back-to-binding-keyword ()
  (or (cb-js-refactor-commands--binding-keyword-at-pt)
      (when (search-backward-regexp (rx symbol-start (group (or "const" "let" "var")) symbol-end))
        (match-string-no-properties 1))))

(defun cb-js-refactor-commands--forward-to-end-of-binder ()
  (when (cb-js-refactor-commands--binding-keyword-at-pt)
    (search-forward "="))

  (search-forward-regexp (rx (or ";" (and symbol-start (or "let" "const" "var" "function") symbol-end))))
  (goto-char (match-beginning 0)))

(defun cb-js-refactor-commands--rewrite-comma-sep-binding-line (binder-keyword target-indent-column)
  ;; Deindent
  (back-to-indentation)
  (delete-horizontal-space)
  (indent-to target-indent-column)


  ;; Insert binding keyword and collapse whitespace.
  (if (cb-js-refactor-commands--binding-keyword-at-pt)
      (forward-symbol 1)
    (insert binder-keyword))
  (just-one-space)
  (forward-symbol 1)
  (just-one-space)

  ;; Replace comma or semicolon at end of line.
  (when (search-forward-regexp (rx (+? nonl) (group (any ",;")) (* space) eol) nil t)
    (replace-match ";" t nil nil 1)))

(defun cb-js-refactor-commands-expand-comma-bindings (start-pos)
  "Rewrite bindings at point from comma-separated to independent bindings.

E.g.:

  var util          = require('util'),
      _             = require('lodash'),
      restify       = require('restify');

Is rewritten to:

  const util = require('util');
  const _ = require('lodash');
  const restify = require('restify');

START-POS is the point in the buffer at which to begin the
rewrite. When called interactively, it is the beginning of the
current line."
  (interactive (list (line-beginning-position)))
  (goto-char start-pos)

  ;; Create a marker pointing to the end of the binding group.
  (cb-js-refactor-commands--forward-to-end-of-binder)
  (let ((marker (make-marker)))
    (set-marker marker (point))

    (cb-js-refactor-commands--back-to-binding-keyword)
    (let ((target-column (current-column)))
      (while (< (point) (marker-position marker))
        (cb-js-refactor-commands--rewrite-comma-sep-binding-line "const" target-column)
        (forward-line)))))

(defun cb-js-refactor-commands--import-start-position ()
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp (rx (or "import" "const" "var" "let")))
    (back-to-indentation)
    (point)))

(defun cb-js-refactor-commands--forward-to-end-of-imports ()
  (cl-labels
      ((current-line () (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

    (let ((continue t))
      (while (and continue (not (eobp)))
        (if (or (string-blank-p (current-line))
                (string-match-p (rx bol (* space) (or "import" "const"))
                                (current-line)))
            (forward-line)
          (setq continue nil)))
      (point))))

(defun cb-js-refactor-commands--import-relative-p (line)
  (when (string-match-p (rx "./") line)
    t))

(defun cb-js-refactor-commands--tidy-import-whitespace (line)
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (when (search-forward-regexp (rx bol (+? nonl) (group (* space)) "=" (group (* space)))
                                 nil t)
      (replace-match " " nil nil nil 1)
      (replace-match " " nil nil nil 2))
    (buffer-string)))

(defun cb-js-refactor-commands-group-and-sort-imports (start-pos)
  "Rewrite bindings at point from comma-separated to independent bindings.

E.g.:

  const restify = require('restify');
  const bar = require('../../bar');
  const util = require('util');
  const foo = require('./foo');

Is rewritten to:

  const restify = require('restify');
  const util = require('util');

  const bar = require('../../bar');
  const foo = require('./foo');

START-POS is the point in the buffer at which to begin the
rewrite. When called interactively, it is the beginning of the
current line."
  (interactive (list (cb-js-refactor-commands--import-start-position)))
  (goto-char start-pos)

  (-let* ((end-pos (cb-js-refactor-commands--forward-to-end-of-imports))
          (region (buffer-substring-no-properties start-pos end-pos))

          ((absolutes relatives)
           (->> (split-string region (rx (any "\r\n")) t (rx space))
                (-map #'cb-js-refactor-commands--tidy-import-whitespace)
                (-partition-by #'cb-js-refactor-commands--import-relative-p)
                (--map (-sort #'string< it)))))

    (goto-char start-pos)
    (delete-region start-pos end-pos)

    (when absolutes
      (insert (string-join absolutes "\n")))

    (when (and relatives absolutes)
      (newline 2))

    (when relatives
      (insert (string-join relatives "\n")))

    (when (or relatives absolutes)
      (newline 2))))

(defun cb-js-refactor-commands-organize-imports (start-pos)
  "Clean up and reorder import declarations at the start of the buffer.

START-POS is the start of the import lines. When called
interactively, it is set to the start of the first import group."
  (interactive (list (cb-js-refactor-commands--import-start-position)))
  (cb-js-refactor-commands-expand-comma-bindings start-pos)
  (cb-js-refactor-commands-group-and-sort-imports start-pos))

(defun cb-js-refactor-commands--enclosing-curlies ()
  (-when-let ((plist &as &plist :op op) (sp-get-enclosing-sexp))
    (if (equal op "{")
        plist
      (sp-backward-up-sexp)
      (cb-js-refactor-commands--enclosing-curlies))))

(defun cb-js-refactor-commands--curlies-contain-object-body-p (beg end)
  (string-match-p (rx bos "{" (* (any "\r\n" space))
                      ;; Interpolated keys
                      (or "[" (* space) (+ (syntax symbol)) (* space) "]")
                      ;; Symbol, number, string, etc.
                      (+ (not (any space)))
                      ":")
                  (buffer-substring beg end)))

(defun cb-js-refactor-commands--object-literal-bounds ()
  (-if-let* (((&plist :beg beg :end end) (cb-js-refactor-commands--enclosing-curlies))
             (valid (cb-js-refactor-commands--curlies-contain-object-body-p beg end)))
      (list beg end)
    (user-error "Not inside an object literal")))

(defun cb-js-refactor-commands-align-object-literal-values (beg end)
  "Line up the values in an object literal.

BEG and END are the bounds of the object in the buffer."
  (interactive (cb-js-refactor-commands--object-literal-bounds))
  (align-regexp beg end ":\\(\\s-*\\)\\s-" 1))

(provide 'cb-js-refactor-commands)

;;; cb-js-refactor-commands.el ends here
