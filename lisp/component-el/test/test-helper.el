;;; test-helper.el --- Setup run before tests.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Declare useful variables.

(eval-and-compile
  (defvar project-root
    (locate-dominating-file default-directory "Cask"))

  (defvar this-directory
    (expand-file-name "test" project-root)))

(add-to-list 'load-path project-root)


;; Initialize test coverage.

(when (require 'undercover nil t)
  (eval '(undercover "*.el")))


;; Load package

(require 'component (expand-file-name "component.el" project-root))


;; Helpers

(defmacro should-assert (form)
  `(let ((debug-on-error nil))
     (should-error ,form :type 'cl-assertion-failed)))


(defvar test-helper-loaded t)

;;; test-helper.el ends here
