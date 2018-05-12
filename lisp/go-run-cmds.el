;;; go-run-cmds.el --- Run commands for go  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defgroup go-run-cmds nil
  "Commands for interacting with tests in go."
  :group 'languages
  :prefix "go-run-cmds-")

(defcustom go-run-cmds-use-gocheck? t
  "Whether to use gocheck. If nil, fall back to `-run`."
  :group 'go-run-cmds
  :type 'boolean)

(defconst go-run-cmds--main-buffer "*go run*")
(defconst go-run-cmds--test-buffer "*go test*")

(defun go-run-cmds--run-test (str)
  (let ((compilation-buffer-name-function (lambda (_) go-run-cmds--test-buffer)))
    (compile (concat "go test " str))))

;;;###autoload
(defun go-run-cmds/run-tests (names)
  "Run all unit tests with NAMES."
  (interactive "sNames: ")
  (go-run-cmds--run-test (shell-quote-argument names)))

;;;###autoload
(defun go-run-cmds/run-package-tests ()
  "Run all tests in the package."
  (interactive)
  (go-run-cmds--run-test ""))

;;;###autoload
(defun go-run-cmds/run-package-tests-nested ()
  "Run all tests in this package and its enclosing packages."
  (interactive)
  (go-run-cmds--run-test "./..."))

;;;###autoload
(defun go-run-cmds/run-test-current-function ()
  "Run tests for the current function."
  (interactive)
  (unless (string-match "_test\\.go" buffer-file-name)
    (user-error "Must be in a _test.go file"))
  (save-excursion
    (let ((test-method (if go-run-cmds-use-gocheck? "-check.f" "-run")))
      (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
      (go-run-cmds--run-test (format "%s='%s'" test-method (match-string 2))))))

;;;###autoload
(defun go-run-cmds/run-test-current-suite ()
  "Run current test suite."
  (interactive)
  (unless (string-match "_test\.go" buffer-file-name)
    (user-error "Must be in a _test.go file to run go-test-current-suite"))
  (unless go-run-cmds-use-gocheck?
    (user-error "Gocheck is needed to test the current suite"))
  (save-excursion
    (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
    (go-run-cmds--run-test (format "-check.f='%s'" (match-string 2)))))

;;;###autoload
(defun go-run-cmds/run-main ()
  "Run the main function in the current buffer."
  (interactive)
  (save-buffer)
  (let ((compilation-buffer-name-function (lambda (_) go-run-cmds--main-buffer)))
    (compile (concat "go run " (shell-quote-argument (buffer-file-name))) t)))

(provide 'go-run-cmds)

;;; go-run-cmds.el ends here
