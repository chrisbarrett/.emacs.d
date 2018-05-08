;;; cb-emacs.el --- Variables relating to core Emacs functionality. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst cb-emacs-pinned-subtrees
  '(("lisp/ensime-emacs" . "v2.0.1")
    ("lisp/mu" . "v1.0")))

;; Config Paths

(defconst cb-emacs-cache-directory
  (concat user-emacs-directory ".cache"))

(defconst cb-emacs-autosave-directory
  (concat user-emacs-directory "autosave"))

(defconst cb-emacs-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst cb-emacs-elpa-directory
  (concat user-emacs-directory "elpa"))

(defconst cb-emacs-config-directory
  (concat user-emacs-directory "config"))

(defconst cb-emacs-site-lisp-directory "~/.nix-profile/share/emacs/site-lisp")

(add-to-list 'load-path cb-emacs-site-lisp-directory)

(provide 'cb-emacs)

;;; cb-emacs.el ends here
