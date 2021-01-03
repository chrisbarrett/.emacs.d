;;; config-tests.el --- Tests for this configuration.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)

(ert-deftest source-directory-set-correctly ()
  (should (string-prefix-p "/nix/store/" find-function-C-source-directory))
  (should (string-suffix-p "/share/emacs/src/" find-function-C-source-directory))
  (should (seq-contains-p (directory-files find-function-C-source-directory) "emacs.c")))

(ert-deftest startup-time-less-than-one-second ()
  (let ((startup-duration-seconds (with-no-warnings emacs-init-duration)))
    (should (< startup-duration-seconds 1))))

;; (provide 'config-tests)
;;; config-tests.el ends here
