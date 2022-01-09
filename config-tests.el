;;; config-tests.el --- Tests for this configuration.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)

(ert-deftest source-directory-set-correctly ()
  (should (string-prefix-p "/nix/store/" find-function-C-source-directory))
  (should (string-match-p "/share/emacs/" find-function-C-source-directory))
  (should (seq-contains-p (directory-files find-function-C-source-directory) "emacs.c")))

(ert-deftest raw-startup-time ()
  (should (< emacs-init-duration 1)))

(ert-deftest startup-with-after-init ()
  (should (< total-startup-duration 10)))

;; (provide 'config-tests)
;;; config-tests.el ends here
