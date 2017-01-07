;;; cb-header-line-format.el --- Functions for constructing the header line format string.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'subr-x)

(autoload 'magit-get-current-branch "magit-git")
(autoload 'projectile-project-p "projectile")

(defgroup cb-header-line-format nil
  "Utilities for constructing the header line."
  :group 'themes
  :prefix "cb-header-line-format-")

(defface cb-header-line-nonemphased-element
  '((t
     (:inherit header-line)))
  "Face for non-emphasised elements in the header line."
  :group 'cb-header-line-format)

(defface cb-header-line-project-name
  '((t
     (:inherit header-line)))
  "Face for project name in header line."
  :group 'cb-header-line-format)

(defface cb-header-line-branch-name
  '((t
     (:inherit header-line)))
  "Face for git branch in header line."
  :group 'cb-header-line-format)


;;; Cache variable lookups to improve speed

(defconst cb-header-line--cache-duration-seconds 10)

(defun cb-header-line--cache-expired? (ts)
  (let ((expiry-time (time-add ts cb-header-line--cache-duration-seconds)))
    (time-less-p expiry-time (current-time))))

;; Cache the git branch.

(defvar-local cb-header-line--branch nil
  "A cons of (write-time . branch-name) or nil")

(defun cb-header-line--update-branch ()
  (let ((time (current-time))
        (branch (magit-get-current-branch)))
    (setq cb-header-line--branch (cons time branch))
    branch))

(defun cb-header-line--current-branch ()
  (require 'magit)
  (-if-let ((ts . branch) cb-header-line--branch)
      (cond
       ((cb-header-line--cache-expired? ts)
        (cb-header-line--update-branch))
       (t
        branch))
    (cb-header-line--update-branch)))

;; Cache the projectile project.
;;
;; Projectile maintains its own cache of project info, but it still does file IO
;; as part of its checks.

(defvar-local cb-header-line--project nil
  "A cons of (write-time . project-name) or nil")

(defun cb-header-line--update-project ()
  (let ((time (current-time))
        (project (when-let (p (projectile-project-p))
                   (file-name-nondirectory (directory-file-name p)))))
    (setq cb-header-line--project (cons time project))
    project))

(defun cb-header-line--current-project ()
  (-if-let ((ts . project) cb-header-line--project)
      (cond
       ((cb-header-line--cache-expired? ts)
        (cb-header-line--update-project))
       (t
        project))
    (cb-header-line--update-project)))


;;; Construction functions

(defun cb-header-line--access-mode-info ()
  (let ((str (concat
              (if buffer-read-only "%" "")
              (if (buffer-modified-p) "M" ""))))
    (propertize (s-pad-right 2 " " str) 'face 'cb-header-line-nonemphased-element)))

(defun cb-header-line--narrowing-info ()
  (if (buffer-narrowed-p) "%n " ""))

(defun cb-header-line--project-info ()
  (cl-flet ((nonemphasised (str) (propertize str 'face 'cb-header-line-nonemphased-element)))
    (let* ((project (cb-header-line--current-project))
           (branch (when project (cb-header-line--current-branch))))
      (cond
       ((and project branch)
        (concat (nonemphasised " <")
                (propertize project 'face 'cb-header-line-project-name)
                (nonemphasised " on ")
                (propertize branch 'face 'cb-header-line-branch-name)
                (nonemphasised "> ")))
       (project
        (concat (nonemphasised " <")
                (propertize project 'face 'cb-header-line-project-name)
                (nonemphasised "> ")))
       (t
        "")))))

(defconst cb-header-line-format
  '(
    ;; Print error on low memory
    "%e"
    " "

    ;; Emacsclient info
    mode-line-client

    ;; Current line, padded
    "Line %2l  "
    (:propertize "%6p " face cb-header-line-nonemphased-element)

    ;; Modification indicator.
    (:eval (cb-header-line--access-mode-info))

    ;; Buffer name, with braces on recursive edit
    "  %[%b%] "

    (:eval (cb-header-line--narrowing-info))
    (:eval (cb-header-line--project-info))

    ;; Global mode string, etc.
    mode-line-misc-info))

(provide 'cb-header-line-format)

;;; cb-header-line-format.el ends here
