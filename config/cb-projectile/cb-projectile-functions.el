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
        (seq-remove (-compose #'cb-projectile-functions-ignored-subdir-p #'car))
        (seq-map #'cadr)
        (seq-sort #'string<))
    projects))

(defun cb-projectile-functions-ignored-subdir-p (project)
  (seq-find (lambda (base)
              (or
               (f-same? base project)
               (f-ancestor-of? base project)))
            cb-projectile-functions-ignored-base-dirs))

(provide 'cb-projectile-functions)

;;; cb-projectile-functions.el ends here
