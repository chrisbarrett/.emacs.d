;;; cb-auto-save.el --- Configure auto-save features.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'cb-emacs)

(defconst cb-auto-save-dir (concat cb-emacs-cache-directory "/auto-save"))
(defconst cb-auto-save-local-dir (concat cb-auto-save-dir "/local"))
(defconst cb-auto-save-remotes-dir (concat cb-auto-save-dir "/remotes"))

(dolist (dir (list cb-auto-save-local-dir cb-auto-save-remotes-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(setq auto-save-file-name-transforms
      `(
        ;; Tramp URLs got to remotes dir.
        ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,cb-auto-save-remotes-dir t)
        ;; Otherwise files go to local dir.
        (".*" ,cb-auto-save-local-dir t)))



(provide 'cb-auto-save)

;;; cb-auto-save.el ends here
