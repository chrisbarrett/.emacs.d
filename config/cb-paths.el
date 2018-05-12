;;; cb-paths.el --- Variables relating to core Emacs functionality. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'seq)



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



(defun cb-paths-initialise (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (git-subtrees
          (seq-filter #'file-directory-p
                      (directory-files cb-emacs-lisp-directory t "^[^.]")))
         (config-subtrees
          (seq-filter #'file-directory-p
                      (directory-files cb-emacs-config-directory t "^[^.]")))

         (updated-load-path
          (append (list
                   cb-emacs-lisp-directory
                   cb-emacs-config-directory
                   cb-emacs-site-lisp-directory)
                  config-subtrees
                  git-subtrees
		  load-path)))

    (setq load-path (seq-filter #'file-directory-p updated-load-path))

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))


(provide 'cb-paths)

;;; cb-paths.el ends here
