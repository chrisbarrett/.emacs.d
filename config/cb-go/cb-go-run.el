;;; cb-go-run.el --- Run commands for go  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defgroup cb-go-run nil
  "Commands for interacting with tests in go."
  :group 'languages
  :prefix "cb-go-run-")

(defcustom cb-go-run-use-gocheck? t
  "Whether to use gocheck. If nil, fall back to `-run`."
  :group 'cb-go-run
  :type 'boolean)

(defconst cb-go-run-main-buffer "*go run*")
(defconst cb-go-run-test-buffer "*go test*")

;;;###autoload
(defun cb-go-run-tests (names)
  "Run all unit tests with NAMES."
  (interactive)
  (let ((compilation-buffer-name-function (lambda (_) cb-go-run-test-buffer)))
    (compile (concat "go test " (shell-quote-argument names)))))

;;;###autoload
(defun cb-go-run-package-tests ()
  "Run all tests in the package."
  (interactive)
  (cb-go-run-tests ""))

;;;###autoload
(defun cb-go-run-package-tests-nested ()
  "Run all tests in this package and its enclosing packages."
  (interactive)
  (cb-go-run-tests "./..."))

;;;###autoload
(defun cb-go-run-test-current-function ()
  "Run tests for the current function."
  (interactive)
  (unless (string-match "_test\\.go" buffer-file-name)
    (user-error "Must be in a _test.go file"))
  (save-excursion
    (let ((test-method (if cb-go-run-use-gocheck? "-check.f" "-run")))
      (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
      (cb-go-run-tests (format "%s='%s'" test-method (match-string 2))))))

;;;###autoload
(defun cb-go-run-test-current-suite ()
  "Run current test suite."
  (interactive)
  (unless (string-match "_test\.go" buffer-file-name)
    (user-error "Must be in a _test.go file to run go-test-current-suite"))
  (unless cb-go-run-use-gocheck?
    (user-error "Gocheck is needed to test the current suite"))
  (save-excursion
    (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
    (cb-go-run-tests (format "-check.f='%s'" (match-string 2)))))

;;;###autoload
(defun cb-go-run-main ()
  "Run the main function in the current buffer."
  (interactive)
  (save-buffer)
  (let ((compilation-buffer-name-function (lambda (_) cb-go-run-main-buffer)))
    (compile (concat "go run " (shell-quote-argument (buffer-file-name))) t)))

(provide 'cb-go-run)

;;; cb-go-run.el ends here
