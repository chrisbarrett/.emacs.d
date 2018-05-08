;;; cb-projectile-functions.el --- Shared functions for working with projectile  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash-functional)
(require 'seq)
(require 'subr-x)
(require 'f)

(defvar cb-projectile-functions-ignored-base-dirs nil)

(defun cb-projectile-functions-cleanup-projects (projects)
  (if (sequencep projects)
      (thread-last projects
        (seq-group-by #'file-truename)
        (seq-remove (-compose #'cb-projectile-functions--removable-project-p #'car))
        (seq-map #'cadr)
        (seq-sort #'string<))
    projects))

(defun cb-projectile-functions--removable-project-p (project)
  (or (cb-projectile-functions-ignored-subdir-p project)
      (not (f-exists? project))))

(defun cb-projectile-functions-ignored-subdir-p (project)
  (thread-last cb-projectile-functions-ignored-base-dirs
    (seq-map #'file-truename)
    (seq-find (lambda (base)
                (or
                 (f-same? base project)
                 (f-ancestor-of? base project))))))

(provide 'cb-projectile-functions)

;;; cb-projectile-functions.el ends here
