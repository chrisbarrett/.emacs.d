;;; js-refactor-commands.el --- Commands for refactoring Javascript  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'seq)
(require 'subr-x)

(autoload 'sp-get-enclosing-sexp "smartparens")
(autoload 'sp-backward-up-sexp "smartparens")



(defun js-refactor-commands--binding-keyword-at-pt ()
  (seq-contains '(const var let) (symbol-at-point)))

(defun js-refactor-commands--back-to-binding-keyword ()
  (or (js-refactor-commands--binding-keyword-at-pt)
      (when (search-backward-regexp (rx symbol-start (group (or "const" "let" "var")) symbol-end))
        (match-string-no-properties 1))))

(defun js-refactor-commands--forward-to-end-of-binder ()
  (when (js-refactor-commands--binding-keyword-at-pt)
    (search-forward "="))

  (search-forward-regexp (rx (or ";" (and symbol-start (or "let" "const" "var" "function") symbol-end))))
  (goto-char (match-beginning 0)))

(defun js-refactor-commands--rewrite-comma-sep-binding-line (binder-keyword target-indent-column)
  ;; Deindent
  (back-to-indentation)
  (delete-horizontal-space)
  (indent-to target-indent-column)


  ;; Insert binding keyword and collapse whitespace.
  (if (js-refactor-commands--binding-keyword-at-pt)
      (forward-symbol 1)
    (insert binder-keyword))
  (just-one-space)
  (forward-symbol 1)
  (just-one-space)

  ;; Replace comma or semicolon at end of line.
  (when (search-forward-regexp (rx (+? nonl) (group (any ",;")) (* space) eol) nil t)
    (replace-match ";" t nil nil 1)))

(defun js-refactor-commands-expand-comma-bindings (start-pos &optional interactive)
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
current line.

If INTERACTIVE is set, raise an error if not at a binding site."
  (interactive (list (line-beginning-position) t))
  (goto-char start-pos)
  (cond ((js-refactor-commands--binding-keyword-at-pt)
         ;; Create a marker pointing to the end of the binding group.
         (js-refactor-commands--forward-to-end-of-binder)
         (let ((marker (make-marker)))
           (set-marker marker (point))

           (js-refactor-commands--back-to-binding-keyword)
           (let ((target-column (current-column)))
             (while (< (point) (marker-position marker))
               (js-refactor-commands--rewrite-comma-sep-binding-line "const" target-column)
               (forward-line)))))
        (interactive
         (user-error "Not an expandable binding site"))))

(defun js-refactor-commands--import-start-position ()
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp (rx bol (* space) (or "import" "const" "var" "let")))
    (back-to-indentation)
    (point)))

(defun js-refactor-commands--import-line-p (line)
  (string-match-p (rx bol (* space)
                      (or "import"
                          (and (or "const" "let" "var") (+ space) (+? nonl) "=" (* space) "require" (* space) "(")))
                  line))

(defun js-refactor-commands--forward-to-end-of-imports ()
  (cl-labels
      ((current-line () (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

    (let ((continue t))
      (while (and continue (not (eobp)))
        (if (or (string-blank-p (current-line))
                (js-refactor-commands--import-line-p (current-line)))
            (forward-line)
          (setq continue nil)))
      (point))))

(defvar js-refactor-commands-root-import-prefixes
  '("actions"
    "components"
    "constants"
    "dispatcher"
    "images"
    "models"
    "routes"
    "selectors"
    "services"
    "stores"
    "types"
    "utils")
  "The names of root directories in the script tree available without qualification.")

(defconst js-refactor-commands--match-relative-import
  (rx-to-string
   `(or "./"
        ;; ... require('./foo')
        (and "require" (* space) "(" (any "\"'") (or "./" "../"))
        ;; import ... from './foo'
        (and bol "import" (+? nonl) "from" (* space) (any "\"'") (or "./" "../")))))

(defconst js-refactor-commands--match-rewriteable-relative-import
  (rx-to-string
   `(or "./"
        ;; ... require('foo')
        (and "require" (* space) "(" (any "\"'") (or ,@js-refactor-commands-root-import-prefixes) "/")
        ;; import ... from 'foo'
        (and bol "import" (+? nonl) "from" (* space) (any "\"'") (or ,@js-refactor-commands-root-import-prefixes) "/"))))

(defun js-refactor-commands--import-kind (line)
  (cond
   ((string-match-p (rx bol (* space) "import" (* space) "type" symbol-end (* space)) line)
    'flow-type)
   ((string-match-p js-refactor-commands--match-relative-import line)
    'relative)
   ((string-match-p js-refactor-commands--match-rewriteable-relative-import line)
    'relative-rewritten)
   (t
    'absolute)))

(defun js-refactor-commands--tidy-import-whitespace (line)
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (when (search-forward-regexp (rx bol (+? nonl) (group (* space)) "=" (group (* space)))
                                 nil t)
      (replace-match " " nil nil nil 1)
      (replace-match " " nil nil nil 2))
    (buffer-string)))

(defun js-refactor-commands--rewrite-require-to-destructure (line)
  (-if-let ((_ kw module member)
            (s-match (rx
                      bol (* space)
                      (group (or "const" "let" "var"))
                      (+ space)
                      (and (not (any "{")) (*? nonl))
                      "=" (* space) "require" (* space) "(" (group (+? nonl)) ")"
                      (* space) "."
                      (group (+? (not (any "(" "."))))
                      ";")
                     line))
      (format "%s { %s } = require(%s);" kw member module)
    line))

(defun js-refactor-commands--import-binder (import-string)
  (-let [(_ binder)
         (s-match (rx (or "import" "const") (+ space) (group (+? nonl)) (* space) (or "from" "="))
                  import-string)]
    binder))

(defun js-refactor-commands--format-imports (import-lines)
  (-let* ((format-import (-compose #'js-refactor-commands--rewrite-require-to-destructure #'js-refactor-commands--tidy-import-whitespace))
          ((&alist 'absolute absolutes 'relative-rewritten relatives-rewritten 'relative relatives 'flow-type types)
           (-group-by #'js-refactor-commands--import-kind (-map format-import import-lines))))
    (cl-labels ((format-group
                 (lines)
                 (when-let* ((sorted-imports (seq-uniq (seq-sort-by #'js-refactor-commands--import-binder #'string< lines))))
                   (concat (string-join sorted-imports "\n") "\n\n"))))
      (concat
       (format-group absolutes)
       (format-group relatives-rewritten)
       (format-group types)
       (format-group relatives)))))

(defun js-refactor-commands-group-and-sort-imports (start-pos)
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
  (interactive (list (js-refactor-commands--import-start-position)))
  (goto-char start-pos)

  (-let* ((end-pos (js-refactor-commands--forward-to-end-of-imports))
          (region (buffer-substring-no-properties start-pos end-pos))
          (import-lines (split-string region (rx (any "\r\n")) t (rx space)))
          (formatted-imports (js-refactor-commands--format-imports import-lines)))

    (goto-char start-pos)
    (delete-region start-pos end-pos)
    (insert formatted-imports)))

(defun js-refactor-commands-organize-imports (start-pos)
  "Clean up and reorder import declarations at the start of the buffer.

START-POS is the start of the import lines. When called
interactively, it is set to the start of the first import group."
  (interactive (list (js-refactor-commands--import-start-position)))
  (js-refactor-commands-expand-comma-bindings start-pos)
  (js-refactor-commands-group-and-sort-imports start-pos))

(defun js-refactor-commands--enclosing-curlies ()
  (-when-let ((plist &as &plist :op op) (sp-get-enclosing-sexp))
    (if (equal op "{")
        plist
      (sp-backward-up-sexp)
      (js-refactor-commands--enclosing-curlies))))

(defun js-refactor-commands--curlies-contain-object-body-p (beg end)
  (string-match-p (rx bos "{" (* (any "\r\n" space))
                      ;; Interpolated keys
                      (or "[" (* space) (+ (syntax symbol)) (* space) "]")
                      ;; Symbol, number, string, etc.
                      (+ (not (any space)))
                      (or ":" ","))
                  (buffer-substring beg end)))

(defun js-refactor-commands--object-literal-bounds ()
  (-if-let* (((&plist :beg beg :end end) (js-refactor-commands--enclosing-curlies))
             (valid (js-refactor-commands--curlies-contain-object-body-p beg end)))
      (list beg end)
    (user-error "Not inside an object literal")))

(defun js-refactor-commands-align-object-literal-values (beg end)
  "Line up the values in an object literal.

E.g.:

  const foo = {
    a: 1,
    b:2,
    c:        3,
    dd: 4,
  }

Is rewritten to:

  const foo = {
    a:  1,
    b:  2,
    c:  3,
    dd: 4,
  }

BEG and END are the bounds of the object in the buffer."
  (interactive (js-refactor-commands--object-literal-bounds))
  (align-regexp beg end ":\\(\\s-*\\)" 1))

(defun js-refactor-commands--seal-object (beg end)
  (save-excursion
    (goto-char (1- end))
    (insert "|")
    (goto-char (1+ beg))
    (insert "|")))

(defun js-refactor-commands--unseal-object (beg end)
  (save-excursion
    (goto-char end)
    (forward-char -1)
    (delete-char -1)
    (goto-char beg)
    (forward-char 1)
    (delete-char 1)))

(defun js-refactor-commands--at-flow-type-p (pos)
  (save-excursion
    (goto-char pos)
    (string-match-p (rx bol (* space) (? "export" (+ space)) "type" symbol-end)
                    (buffer-substring (line-beginning-position) (line-end-position)))))

(defun js-refactor-commands--strict-object-type-bounds ()
  (-when-let ((plist &as &plist :op op) (sp-get-enclosing-sexp))
    (if (equal op "{|")
        plist
      (sp-backward-up-sexp)
      (js-refactor-commands--strict-object-type-bounds))))

(defun js-refactor-commands-toggle-sealed-object-type ()
  "Toggle between a sealed or unsealed flow type.

BEG and END are the bounds of the object type at point."
  (interactive)
  (save-excursion
    (-let* (((object-bounds &as &plist :beg b1 :end e1) (save-excursion (js-refactor-commands--enclosing-curlies)))
            ((strict-type-bounds &as &plist :beg b2 :end e2) (save-excursion (js-refactor-commands--strict-object-type-bounds)))
            (beg (or b1 b2))
            (end (or e1 e2)))
      (cond
       ((not beg)
        (user-error "Not at a flow object type declaration"))
       ((not (js-refactor-commands--at-flow-type-p beg))
        (user-error "Not at a flow type declaration"))
       (strict-type-bounds
        (js-refactor-commands--unseal-object beg end))
       (object-bounds
        (js-refactor-commands--seal-object beg end))))))

(provide 'js-refactor-commands)

;;; js-refactor-commands.el ends here
