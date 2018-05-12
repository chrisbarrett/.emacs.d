;;; projectile-funcs.el --- Shared functions for working with projectile  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash-functional)
(require 'seq)
(require 'subr-x)
(require 'f)

(defvar projectile-funcs-ignored-base-dirs nil)

(defun projectile-funcs-cleanup-projects (projects)
  (if (sequencep projects)
      (thread-last projects
        (seq-group-by #'file-truename)
        (seq-remove (-compose #'projectile-funcs--removable-project-p #'car))
        (seq-map #'cadr)
        (seq-sort #'string<))
    projects))

(defun projectile-funcs--removable-project-p (project)
  (or (projectile-funcs-ignored-subdir-p project)
      (not (f-exists? project))))

(defun projectile-funcs-ignored-subdir-p (project)
  (thread-last projectile-funcs-ignored-base-dirs
    (seq-map #'file-truename)
    (seq-find (lambda (base)
                (or
                 (f-same? base project)
                 (f-ancestor-of? base project))))))

(provide 'projectile-funcs)

;;; projectile-funcs.el ends here
