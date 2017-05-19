;;-*- Mode: Emacs-Lisp -*-
;;; Cask --- ENSIME project definition

;; Copyright (C) 2015 Sam Halliday

;; Author: Sam Halliday <Sam.Halliday@gmail.com>

;;; Commentary:
;;
;;  Cask is a package manager for emacs lisp projects, this generates
;;  the ensime-pkg.el file and could be our test runner in the future.
;;
;;  See http://cask.readthedocs.org/en/latest/guide/dsl.html for more
;;  information about Cask.
;;
;;    cask pkg-file
;;
;;    cask update
;;    cask install
;;
;;  are particularly useful commands (update/install is for flycheck-cask).
;;
;;; Code:

(source melpa-stable)

(package-file "ensime.el")

(development
 ;; optional dependencies (used in the tests)
 (depends-on "use-package")
 (depends-on "auto-complete")
 (depends-on "helm")
 (depends-on "ivy")
 ;;(depends-on "expand-region")
 (depends-on "f")
 (depends-on "ert-runner")
 (depends-on "ecukes")
 (depends-on "espuds")
 ; genuine dev dependencies
 (depends-on "undercover"))

;;; Cask ends here
