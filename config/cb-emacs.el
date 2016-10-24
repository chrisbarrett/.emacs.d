;;; cb-emacs.el --- Variables relating to core Emacs functionality. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst cb-emacs-cache-directory
  (concat user-emacs-directory ".cache"))

(defconst cb-emacs-autosave-directory
  (concat user-emacs-directory "autosave"))

(defconst cb-emacs-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst cb-emacs-config-directory
  (concat user-emacs-directory "config"))

(provide 'cb-emacs)

;;; cb-emacs.el ends here
