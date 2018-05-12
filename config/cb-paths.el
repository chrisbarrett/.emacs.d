;;; cb-paths.el --- Variables relating to core Emacs functionality. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'seq)



(defconst cb-paths-cache-directory
  (concat user-emacs-directory ".cache"))

(defconst cb-paths-autosave-directory
  (concat user-emacs-directory "autosave"))

(defconst cb-paths-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst cb-paths-elpa-directory
  (concat user-emacs-directory "elpa"))

(defconst cb-paths-config-directory
  (concat user-emacs-directory "config"))

(defconst cb-paths-site-lisp-directory "~/.nix-profile/share/emacs/site-lisp")



(defun cb-paths-initialise (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (git-subtrees
          (seq-filter #'file-directory-p
                      (directory-files cb-paths-lisp-directory t "^[^.]")))
         (config-subtrees
          (seq-filter #'file-directory-p
                      (directory-files cb-paths-config-directory t "^[^.]")))

         (updated-load-path
          (append (list
                   cb-paths-lisp-directory
                   cb-paths-config-directory
                   cb-paths-site-lisp-directory)
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
