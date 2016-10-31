;;; cb-rust-faces.el --- Font locking tweaks for rust.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defgroup cb-rust-faces nil
  "Font locking tweaks for rust."
  :group 'languages
  :prefix "cb-rust-faces-")

(defface cb-rust-faces-macro
  '((default (:inherit font-lock-preprocessor-face)))
  "Face for macros like `println!'"
  :group 'cb-rust-faces)

(defface cb-rust-faces-bool
  '((default (:inherit font-lock-keyword-face)))
  "Face for true and false literals."
  :group 'cb-rust-faces)


(defconst cb-rust-faces--macro-rx
  (rx word-start (group (+ (syntax word)) "!")))

(defconst cb-rust-faces--bool-rx
  (rx word-start (group (or "true" "false")) word-end))

(defconst cb-rust-faces--static-scope-rx
  (rx "'" word-start (group "static") word-end))

(font-lock-add-keywords
 'rust-mode
 `(
   ;; Apply face to distinguish macro invocations from directives.
   (,cb-rust-faces--macro-rx 1 'cb-rust-faces-macro)

   ;; Remove keyword face from references to `static' scope.
   (,cb-rust-faces--static-scope-rx 1 'font-lock-variable-name-face)

   ;; Distinguish `true' and `false' literals from other keywords.
   (,cb-rust-faces--bool-rx 1 'cb-rust-faces-bool)))

(provide 'cb-rust-faces)

;;; cb-rust-faces.el ends here
