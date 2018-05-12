;;; rust-faces.el --- Font locking tweaks for rust.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defgroup rust-faces nil
  "Font locking tweaks for rust."
  :group 'languages
  :prefix "rust-faces-")

(defface rust-faces-macro
  '((default (:inherit font-lock-preprocessor-face)))
  "Face for macros like `println!'"
  :group 'rust-faces)

(defface rust-faces-bool
  '((default (:inherit font-lock-keyword-face)))
  "Face for true and false literals."
  :group 'rust-faces)


(defconst rust-faces--macro-rx
  (rx word-start (group (+ (syntax word)) "!")))

(defconst rust-faces--bool-rx
  (rx word-start (group (or "true" "false")) word-end))

(defconst rust-faces--static-scope-rx
  (rx "'" word-start (group "static") word-end))

(font-lock-add-keywords
 'rust-mode
 `(
   ;; Apply face to distinguish macro invocations from directives.
   (,rust-faces--macro-rx 1 'rust-faces-macro)

   ;; Remove keyword face from references to `static' scope.
   (,rust-faces--static-scope-rx 1 'font-lock-variable-name-face)

   ;; Distinguish `true' and `false' literals from other keywords.
   (,rust-faces--bool-rx 1 'rust-faces-bool)))

(provide 'rust-faces)

;;; rust-faces.el ends here
