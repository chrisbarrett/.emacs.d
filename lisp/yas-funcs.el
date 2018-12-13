;;; yas-funcs.el --- Functions for yasnippets.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'subr-x)
(require 'thingatpt)

(autoload 'sp-get-enclosing-sexp "smartparens")

;; Declaration of dynamic variable to satisfy byte-compiler.
(defvar yas-text nil)

(defun yas-funcs-bolp ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (s-matches? (rx bol (* space) (* word) (* space) eol)
              (buffer-substring (line-beginning-position) (line-end-position))))


;;; Haskell

(cl-defun yas-funcs-hs-constructor-name (&optional (text yas-text))
  (car (s-split (rx space) text)))



;;; Elisp

(defun yas-funcs-el-custom-group ()
  "Find the first group defined in the current file.
Fall back to the file name sans extension."
  (or
   (cadr (s-match (rx "(defgroup" (+ space) (group (+ (not
                                                       space))))
                  (buffer-string)))
   (cadr (s-match (rx ":group" (+ space) "'" (group (+ (any "-" alnum))))
                  (buffer-string)))
   (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

(defun yas-funcs-el-autoload-file (sym)
  (if-let* ((file (symbol-file (if (stringp sym) (intern sym) sym))))
      (file-name-sans-extension (file-name-nondirectory file))
    ""))

(defun yas-funcs-el-at-line-above-decl-p ()
  (save-excursion
    (forward-line)
    (back-to-indentation)
    (thing-at-point-looking-at (rx (* space) "("
                                   (or "cl-defun" "defun" "defvar" "defconst"
                                       "define-minor-mode"
                                       "define-globalized-minor-mode"
                                       "define-derived-mode")))))

(defun yas-funcs-el-package-prefix ()
  (if (bound-and-true-p nameless-current-name)
      nameless-current-name
    (f-base (or (buffer-file-name) (buffer-name)))))


;;; JS

(cl-defun yas-funcs-js-module-name-for-binding (&optional (text yas-text))
  (pcase text
    ('nil      "")
    (""        "")
    ("Promise" "bluebird")
    ("assert"  "power-assert")
    ("VError"  "verror")
    ("styles"  "./styles.css")
    ("stampit"   "@stamp/it")
    ("Configure" "@stamp/configure")
    ("Required"  "@stamp/required")
    ("_"       "lodash")

    ((guard (s-contains? "{" text))
     "")
    (s
     (s-downcase (s-dashed-words s)))))

(defun yas-funcs-js-ctor-body (argstring)
  (when argstring
    (thread-last argstring
      (s-split (rx (or "," ".")))
      (-map #'s-trim)
      (-remove #'s-blank?)
      (--map (format "this.%s = %s;" it it))
      (s-join "\n"))))


;;; Rust

(defun yas-funcs-sp-find-enclosing-pair (pred &optional prev-beg)
  "Search up for an enclosing pair satisfying PRED.

PRED is a function, called with the current pair.

PREV-BEG is a buffer position used to track the position of the
previous match and abort if no progress is made."
  (save-excursion
    (-when-let ((pair &as &plist :beg beg) (sp-get-enclosing-sexp))
      (goto-char beg)
      (cond
       ((eq prev-beg beg))
       ((funcall pred pair)
        beg)
       (t
        (yas-funcs-sp-find-enclosing-pair pred beg))))))

(defun yas-funcs-rs-in-fn-p ()
  (yas-funcs-sp-find-enclosing-pair
   (-lambda ((&plist :beg beg :op op))
     (when (equal op "{")
       (save-excursion
         (goto-char beg)
         (s-matches-p (rx bol (* space) (? "pub" (+ space)) "fn" symbol-end)
                      (buffer-substring (line-beginning-position)
                                        (line-end-position))))))))

(defun yas-funcs-rs-in-test-module-p ()
  (yas-funcs-sp-find-enclosing-pair
   (-lambda ((&plist :beg beg :op op))
     (when (equal op "{")
       (save-excursion
         (goto-char beg)
         (s-matches-p (rx bol (* space) "mod" (+ space) "tests" symbol-end)
                      (buffer-substring (line-beginning-position)
                                        (line-end-position))))))))

(defun yas-funcs-rs-bol-or-after-access-kw-p ()
  (save-excursion
    (save-restriction
      ;; Move past access modifier.
      (goto-char (line-beginning-position))
      (search-forward-regexp (rx bow "pub" eow (* space)) (line-end-position) t)
      (narrow-to-region (point) (line-end-position))
      (yas-funcs-bolp))))

(defun yas-funcs-rs-relevant-struct-name ()
  "Search backward for the name of the last struct defined in this file."
  (save-match-data
    (if (search-backward-regexp (rx (or "enum" "struct") (+ space)
                                    (group (+ (not (any ";" "(" "{")))))
                                nil t)
        (s-trim (match-string 1))
      "Name")))


;;; Scala

(defun yas-funcs-scala-test-fixture-name ()
  (or (ignore-errors (f-filename (f-no-ext (buffer-file-name))))
      "TestFixture"))



;;; C#

(defun yas-funcs-csharp-ctor-name ()
  (save-excursion
    (if (search-backward-regexp (rx "class" symbol-end (+ space) symbol-start (group (+? nonl)) symbol-end) nil t)
        (match-string-no-properties 1)
      "ClassName")))

(defun yas-funcs-csharp-default-class-name ()
  (let ((default (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp (rx-to-string `(and "class" (+ space) ,default)) nil t)
          "Name"
        default))))


;;; Java

(defun yas-funcs-java-ctor-name ()
  (save-excursion
    (if (search-backward-regexp (rx "class" symbol-end (+ space) symbol-start (group (+? nonl)) symbol-end))
        (match-string-no-properties 1)
      "ClassName")))

(defun yas-funcs--java-ctor-parse-param-string (param-string)
  (--keep (-let* ((it (s-trim it))
                  (tokens (s-split (rx (+ space)) it))
                  (type (s-join " " (-butlast tokens)))
                  (ident (-last-item tokens)))
            (unless (-any #'string-blank-p (list ident type))
              (list :type type :ident ident)))
          (s-split (rx (any ",")) param-string t)))

(defun yas-funcs-java-ctor-body (param-string)
  (let ((lines (-map
                (-lambda ((&plist :ident ident))
                  (format "_%s = %s;" ident ident))
                (yas-funcs--java-ctor-parse-param-string param-string))))
    (s-join "\n" lines)))

(defun yas-funcs-java-ctor-fields (param-string)
  (let ((lines (-map
                (-lambda ((&plist :ident ident :type type))
                  (format "private %s _%s = null;" type ident))
                (yas-funcs--java-ctor-parse-param-string param-string))))
    (s-join "\n" lines)))

(provide 'yas-funcs)

;;; yas-funcs.el ends here
