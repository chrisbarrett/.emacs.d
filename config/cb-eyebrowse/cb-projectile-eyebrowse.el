;;; cb-projectile-eyebrowse.el --- Integrate projectile with eyebrowse.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'projectile)
(require 'dash)
(require 'eyebrowse)
(require 'f)

(defun cb-projectile-eyebrowse--slot-name-pairs (configs)
  (-map (-lambda ((slot _ name))
          (cons slot name))
        configs))

(defun cb-projectile-eyebrowse--slot-for-project (project-root)
  (car (--find (equal project-root (cdr it))
               (cb-projectile-eyebrowse--slot-name-pairs (eyebrowse--get 'window-configs)))))

(defun cb-projectile-eyebrowse--read-project ()
  (-if-let (projects (projectile-relevant-known-projects))
      (completing-read "Switch to project: " projects)
    (user-error "There are no known projects")))

(defun cb-projectile-eyebrowse-switch-to-project (project-root)
  "Switch to the project at PROJECT-ROOT."
  (interactive (list (cb-projectile-eyebrowse--read-project)))
  (-if-let (slot (cb-projectile-eyebrowse--slot-for-project project-root))
      (eyebrowse-switch-to-window-config slot)
    (let ((slot (eyebrowse-free-slot (-map #'car (eyebrowse--get 'window-configs)))))
      (eyebrowse-switch-to-window-config slot)
      (eyebrowse-rename-window-config slot project-root)
      (projectile-switch-project-by-name project-root))))

(provide 'cb-projectile-eyebrowse)

;;; cb-projectile-eyebrowse.el ends here
