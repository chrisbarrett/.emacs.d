;;; header-line-format.el --- Functions for constructing the header line format string.  -*- lexical-binding: t; -*-

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

(defgroup header-line-format nil
  "Utilities for constructing the header line."
  :group 'themes
  :prefix "header-line-format-")

(defface header-line-format-nonemphasised-element
  '((t
     (:inherit header-line)))
  "Face for non-emphasised elements in the header line."
  :group 'header-line-format)

(defface header-line-format-emphasised-element
  '((t
     (:inherit header-line)))
  "Face for accented elements in the header line."
  :group 'header-line-format)

(defface header-line-format-project-name
  '((t
     (:inherit header-line)))
  "Face for project name in header line."
  :group 'header-line-format)

(defface header-line-format-branch-name
  '((t
     (:inherit header-line)))
  "Face for git branch in header line."
  :group 'header-line-format)

(defface header-line-format-host-name
  '((t
     (:inherit header-line)))
  "Face for host-name in header line."
  :group 'header-line-format)

;;; Helper for testing if window selected.

(defvar header-line-format--window-for-redisplay nil
  "The window currently being redisplayed.")

(defun header-line-format--set-window-for-redisplay (_)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq header-line-format--window-for-redisplay (selected-window))))

(add-function :before pre-redisplay-function #'header-line-format--set-window-for-redisplay)

(defun header-line-format--window-selected? ()
  (eq header-line-format--window-for-redisplay (get-buffer-window)))

;; Cache the git branch.

(defun header-line-format--current-branch-internal (_directory)
  (magit-get-current-branch))

(memoize #'header-line-format--current-branch-internal "3 seconds")

(defun header-line-format--current-branch ()
  (require 'magit)
  (header-line-format--current-branch-internal default-directory))

;; Cache the projectile project.
;;
;; Projectile maintains its own cache of project info, but it still does file IO
;; as part of its checks.

(defun header-line-format--current-project-internal (_directory)
  (projectile-project-p))

(memoize #'header-line-format--current-project-internal "10 seconds")

(defun header-line-format--current-project ()
  (header-line-format--current-project-internal default-directory))

;;; Construction functions

(defun header-line-format--nonemphasised (str)
  (propertize str 'face 'header-line-format-nonemphasised-element))

(defun header-line-format--access-mode-info ()
  (let ((str (concat
              (if (and (buffer-file-name) (file-remote-p (buffer-file-name))) "@" "")
              (if buffer-read-only "%" "")
              (if (buffer-modified-p) "*" ""))))
    (propertize (s-pad-right 2 " " str) 'face 'header-line-format-emphasised-element)))

(defun header-line-format--narrowing-info ()
  (if (buffer-narrowed-p)
      (propertize " (Narrowed) " 'face 'header-line-format-emphasised-element)
    ""))

(defun header-line-format--project-info ()
  (let* ((project (header-line-format--current-project))
         (project (when project (directory-file-name project)))
         (project-root-name (when project (file-name-nondirectory project)))
         (branch (when project (header-line-format--current-branch)))
         (subdir (when project (s-chop-prefix project (directory-file-name (file-truename default-directory))))))
    (cond
     ((and project branch)
      (concat (header-line-format--nonemphasised " (")
              (propertize project-root-name 'face 'header-line-format-project-name)
              (header-line-format--nonemphasised subdir)
              (header-line-format--nonemphasised " on ")
              (all-the-icons-octicon "git-branch" :v-adjust 0.1 :height 0.9)
              " "
              (propertize branch 'face 'header-line-format-branch-name)
              (header-line-format--nonemphasised ") ")))
     (project
      (concat (header-line-format--nonemphasised " (in ")
              (propertize project-root-name 'face 'header-line-format-project-name)
              (header-line-format--nonemphasised ") ")))
     (t
      ""))))

(defun header-line-format--host-info ()
  (concat
   (header-line-format--nonemphasised " (at ")
   (propertize (and (boundp 'tramp-current-host) tramp-current-host) 'face 'header-line-format-host-name)
   (header-line-format--nonemphasised ") ")))

(defun header-line-format--context-info ()
  (cond
   ((not (header-line-format--window-selected?))
    "")
   ((file-remote-p default-directory)
    "")
   (t
    (header-line-format--project-info))))

(defun header-line-format--major-mode-icon ()
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

(defun header-line-format--major-mode-info ()
  (if (header-line-format--window-selected?)
      (header-line-format--major-mode-icon)
    "   "))

(defun header-line-format--buffer-name ()
  (if (header-line-format--window-selected?)
      (buffer-name)
    (propertize (buffer-name) 'face 'header-line-format-nonemphasised-element)))

(defun header-line-format--line-info ()
  (let ((str "%2l"))
    (if (header-line-format--window-selected?)
        str
      (propertize str 'face 'header-line-format-nonemphasised-element))))

(defconst header-line-format--flycheck-icon (all-the-icons-material "error_outline"))
(defconst header-line-format--tree-icon (all-the-icons-octicon "file-directory" :v-adjust 0.05))
(defconst header-line-format--ilist-icon (all-the-icons-fileicon "api-blueprint" :v-adjust 0.05))
(defconst header-line-format--org-icon (all-the-icons-icon-for-mode 'org-mode :v-adjust 0.05))

(defconst header-line-format
  '((:eval (pcase (buffer-name)
             ;; Special buffers
             ((or "*Help*" (guard (string-match-p (rx bos "*helpful ") (buffer-name))))
              (concat " " (header-line-format--major-mode-icon) " Help"))
             ("*Flycheck errors*"
              (concat " " header-line-format--flycheck-icon " Errors"))
             ("*Org Select*"
              (concat " " header-line-format--org-icon " *Org Select*"))
             ("*compilation*"
              (concat " " (header-line-format--major-mode-icon) " Compile"))
             ((guard (string-match-p (rx "*Treemacs-Framebuffer-") (buffer-name)))
              (concat " " header-line-format--tree-icon " Filesystem"))
             ("*Ilist*"
              (concat " " header-line-format--ilist-icon " Definitions"))
             ((guard (string-match-p (rx "-popup*" eos) (buffer-name)))
              (concat " " (header-line-format--major-mode-icon) (buffer-name)))

             (_
              '(
                ;; Print error on low memory
                "%e"
                " "

                ;; Emacsclient info
                mode-line-client

                ;; Major mode icon
                (:eval (header-line-format--major-mode-info))

                ;; Current line, padded
                (:eval (header-line-format--line-info))
                "  "
                (:propertize "%6p " face header-line-format-nonemphasised-element)

                ;; Modification indicator.
                (:eval (header-line-format--access-mode-info))

                ;; Buffer name, with braces on recursive edit
                "  %[" (:eval (header-line-format--buffer-name)) "%] "

                (:eval (header-line-format--narrowing-info))

                (:eval (header-line-format--context-info))))))))

(provide 'header-line-format)

;;; header-line-format.el ends here
