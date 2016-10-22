;;; cb-emacs.el --- Variables relating to core Emacs functionality. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst cb-emacs-cache-directory
  (concat user-emacs-directory ".cache/"))

(defconst cb-emacs-autosave-directory
  (concat user-emacs-directory "autosave/"))


(provide 'cb-emacs)

;;; cb-emacs.el ends here
