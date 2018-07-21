;;; mode-line-format.el --- Functions for constructing the mode line format string.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett, Raghuvir Kasturi

;; Authors: Chris Barrett <chris+emacs@walrus.cool>, Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(require 'all-the-icons)
(require 'dash)
(require 'memoize)
(require 's)

(autoload 'magit-get-current-branch "magit-git")
(autoload 'projectile-project-p "projectile")

;;; Faces

(defgroup mode-line-format nil
  "Utilities for constructing the mode line."
  :group 'themes
  :prefix "mode-line-format-")

(defface mode-line-format-nonemphasised-element
  '((t
     (:inherit mode-line)))
  "Face for non-emphasised elements in the mode line."
  :group 'mode-line-format)

(defface mode-line-format-emphasised-element
  '((t
     (:inherit mode-line)))
  "Face for accented elements in the mode line."
  :group 'mode-line-format)

(defface mode-line-format-project-name
  '((t
     (:inherit mode-line)))
  "Face for project name in mode line."
  :group 'mode-line-format)

(defface mode-line-format-branch-name
  '((t
     (:inherit mode-line)))
  "Face for git branch in mode line."
  :group 'mode-line-format)

(defface mode-line-format-host-name
  '((t
     (:inherit mode-line)))
  "Face for host-name in mode line."
  :group 'mode-line-format)

;;; Helper for testing if window selected.

(defvar mode-line-format--window-for-redisplay nil
  "The window currently being redisplayed.")

(defun mode-line-format--set-window-for-redisplay (_)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq mode-line-format--window-for-redisplay (selected-window))))

(add-function :before pre-redisplay-function #'mode-line-format--set-window-for-redisplay)

(defun mode-line-format--window-selected? ()
  (eq mode-line-format--window-for-redisplay (get-buffer-window)))

;; Cache the git branch.

(defun mode-line-format--current-branch-internal (_directory)
  (magit-get-current-branch))

(memoize #'mode-line-format--current-branch-internal "3 seconds")

(defun mode-line-format--current-branch ()
  (require 'magit)
  (mode-line-format--current-branch-internal default-directory))

;; Cache the projectile project.
;;
;; Projectile maintains its own cache of project info, but it still does file IO
;; as part of its checks.

(defun mode-line-format--current-project-internal (_directory)
  (projectile-project-p))

(memoize #'mode-line-format--current-project-internal "10 seconds")

(defun mode-line-format--current-project ()
  (mode-line-format--current-project-internal default-directory))

;;; Construction functions

(defun mode-line-format--nonemphasised (str)
  (propertize str 'face 'mode-line-format-nonemphasised-element))

(defun mode-line-format--access-mode-info ()
  (let ((str (concat
              (if (and (buffer-file-name) (file-remote-p (buffer-file-name))) "@" "")
              (if buffer-read-only "%" "")
              (if (buffer-modified-p) "*" ""))))
    (propertize (s-pad-right 2 " " str) 'face 'mode-line-format-emphasised-element)))

(defun mode-line-format--narrowing-info ()
  (if (buffer-narrowed-p)
      (propertize " (Narrowed) " 'face 'mode-line-format-emphasised-element)
    ""))

(defun mode-line-format--project-info ()
  (let* ((project (mode-line-format--current-project))
         (project (when project (directory-file-name project)))
         (project-root-name (when project (file-name-nondirectory project)))
         (branch (when project (mode-line-format--current-branch)))
         (subdir (when project (s-chop-prefix project (directory-file-name (file-truename default-directory))))))
    (cond
     ((and project branch)
      (concat (mode-line-format--nonemphasised " (")
              (propertize project-root-name 'face 'mode-line-format-project-name)
              (mode-line-format--nonemphasised subdir)
              (mode-line-format--nonemphasised " on ")
              (all-the-icons-octicon "git-branch" :v-adjust 0.1 :height 0.9)
              " "
              (propertize branch 'face 'mode-line-format-branch-name)
              (mode-line-format--nonemphasised ") ")))
     (project
      (concat (mode-line-format--nonemphasised " (in ")
              (propertize project-root-name 'face 'mode-line-format-project-name)
              (mode-line-format--nonemphasised ") ")))
     (t
      ""))))

(defun mode-line-format--host-info ()
  (concat
   (mode-line-format--nonemphasised " (at ")
   (propertize (and (boundp 'tramp-current-host) tramp-current-host) 'face 'mode-line-format-host-name)
   (mode-line-format--nonemphasised ") ")))

(defun mode-line-format--context-info ()
  (cond
   ((not (mode-line-format--window-selected?))
    "")
   ((file-remote-p default-directory)
    "")
   (t
    (mode-line-format--project-info))))

(defun mode-line-format--major-mode-icon ()
  (let* ((v-adjust
          (cond
           ((derived-mode-p 'web-mode) 0.05)
           ((derived-mode-p 'emacs-lisp-mode) -0.1)
           (t 0.05)))
         (icon
          (if (derived-mode-p 'web-mode)
              (all-the-icons-icon-for-file (or (ignore-errors (buffer-file-name))
                                               (buffer-name))
                                           :v-adjust v-adjust)
            (all-the-icons-icon-for-mode major-mode :v-adjust v-adjust))))

    (concat (if (symbolp icon) "" icon) " ")))

(defun mode-line-format--major-mode-info ()
  (if (mode-line-format--window-selected?)
      (mode-line-format--major-mode-icon)
    "   "))

(defun mode-line-format--buffer-name ()
  (if (mode-line-format--window-selected?)
      (buffer-name)
    (propertize (buffer-name) 'face 'mode-line-format-nonemphasised-element)))

(defun mode-line-format--line-info ()
  (let ((str "%2l"))
    (if (mode-line-format--window-selected?)
        str
      (propertize str 'face 'mode-line-format-nonemphasised-element))))

(defconst mode-line-format--flycheck-icon (all-the-icons-material "error_outline"))
(defconst mode-line-format--tree-icon (all-the-icons-octicon "file-directory" :v-adjust 0.05))
(defconst mode-line-format--ilist-icon (all-the-icons-fileicon "api-blueprint" :v-adjust 0.05))
(defconst mode-line-format--org-icon (all-the-icons-icon-for-mode 'org-mode :v-adjust 0.05))

(defun mode-line-format ()
  '((:eval (pcase (buffer-name)
             ;; Special buffers
             ((or "*Help*" (guard (string-match-p (rx bos "*helpful ") (buffer-name))))
              (concat " " (mode-line-format--major-mode-icon) " Help"))
             ("*Flycheck errors*"
              (concat " " mode-line-format--flycheck-icon " Errors"))
             ("*Org Select*"
              (concat " " mode-line-format--org-icon " *Org Select*"))
             ("*compilation*"
              (concat " " (mode-line-format--major-mode-icon) " Compile"))
             ((guard (string-match-p (rx "*Treemacs-Framebuffer-") (buffer-name)))
              (concat " " mode-line-format--tree-icon " Filesystem"))
             ("*Ilist*"
              (concat " " mode-line-format--ilist-icon " Definitions"))
             ((guard (string-match-p (rx "-popup*" eos) (buffer-name)))
              (concat " " (mode-line-format--major-mode-icon) (buffer-name)))

             (_
              '(
                ;; Print error on low memory
                "%e"
                " "

                ;; Emacsclient info
                mode-line-client

                ;; Major mode icon
                (:eval (mode-line-format--major-mode-info))

                ;; Current line, padded
                (:eval (mode-line-format--line-info))
                "  "
                (:propertize "%6p " face mode-line-format-nonemphasised-element)

                ;; Modification indicator.
                (:eval (mode-line-format--access-mode-info))

                ;; Buffer name, with braces on recursive edit
                "  %[" (:eval (mode-line-format--buffer-name)) "%] "

                (:eval (mode-line-format--narrowing-info))

                (:eval (mode-line-format--context-info))))))))

(provide 'mode-line-format)

;;; mode-line-format.el ends here
