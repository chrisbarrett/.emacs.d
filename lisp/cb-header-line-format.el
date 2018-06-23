;;; cb-header-line-format.el --- Functions for constructing the header line format string.  -*- lexical-binding: t; -*-

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

(defgroup cb-header-line-format nil
  "Utilities for constructing the header line."
  :group 'themes
  :prefix "cb-header-line-format-")

(defface cb-header-line-format-nonemphasised-element
  '((t
     (:inherit header-line)))
  "Face for non-emphasised elements in the header line."
  :group 'cb-header-line-format)

(defface cb-header-line-format-emphasised-element
  '((t
     (:inherit header-line)))
  "Face for accented elements in the header line."
  :group 'cb-header-line-format)

(defface cb-header-line-format-project-name
  '((t
     (:inherit header-line)))
  "Face for project name in header line."
  :group 'cb-header-line-format)

(defface cb-header-line-format-branch-name
  '((t
     (:inherit header-line)))
  "Face for git branch in header line."
  :group 'cb-header-line-format)

(defface cb-header-line-format-host-name
  '((t
     (:inherit header-line)))
  "Face for host-name in header line."
  :group 'cb-header-line-format)

;;; Helper for testing if window selected.

(defvar cb-header-line-format--window-for-redisplay nil
  "The window currently being redisplayed.")

(defun cb-header-line-format--set-window-for-redisplay (_)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq cb-header-line-format--window-for-redisplay (selected-window))))

(add-function :before pre-redisplay-function #'cb-header-line-format--set-window-for-redisplay)

(defun cb-header-line-format--window-selected? ()
  (eq cb-header-line-format--window-for-redisplay (get-buffer-window)))

;; Cache the git branch.

(defun cb-header-line-format--current-branch-internal (_directory)
  (magit-get-current-branch))

(memoize #'cb-header-line-format--current-branch-internal "3 seconds")

(defun cb-header-line-format--current-branch ()
  (require 'magit)
  (cb-header-line-format--current-branch-internal default-directory))

;; Cache the projectile project.
;;
;; Projectile maintains its own cache of project info, but it still does file IO
;; as part of its checks.

(defun cb-header-line-format--current-project-internal (_directory)
  (projectile-project-p))

(memoize #'cb-header-line-format--current-project-internal "10 seconds")

(defun cb-header-line-format--current-project ()
  (cb-header-line-format--current-project-internal default-directory))

;;; Construction functions

(defun cb-header-line-format--nonemphasised (str)
  (propertize str 'face 'cb-header-line-format-nonemphasised-element))

(defun cb-header-line-format--access-mode-info ()
  (let ((str (concat
              (if (and (buffer-file-name) (file-remote-p (buffer-file-name))) "@" "")
              (if buffer-read-only "%" "")
              (if (buffer-modified-p) "*" ""))))
    (propertize (s-pad-right 2 " " str) 'face 'cb-header-line-format-emphasised-element)))

(defun cb-header-line-format--narrowing-info ()
  (if (buffer-narrowed-p)
      (propertize " (Narrowed) " 'face 'cb-header-line-format-emphasised-element)
    ""))

(defun cb-header-line-format--project-info ()
  (let* ((project (cb-header-line-format--current-project))
         (project (when project (directory-file-name project)))
         (project-root-name (when project (file-name-nondirectory project)))
         (branch (when project (cb-header-line-format--current-branch)))
         (subdir (when project (s-chop-prefix project (directory-file-name (file-truename default-directory))))))
    (cond
     ((and project branch)
      (concat (cb-header-line-format--nonemphasised " (")
              (propertize project-root-name 'face 'cb-header-line-format-project-name)
              (cb-header-line-format--nonemphasised subdir)
              (cb-header-line-format--nonemphasised " on ")
              (all-the-icons-octicon "git-branch" :v-adjust 0.1 :height 0.9)
              " "
              (propertize branch 'face 'cb-header-line-format-branch-name)
              (cb-header-line-format--nonemphasised ") ")))
     (project
      (concat (cb-header-line-format--nonemphasised " (in ")
              (propertize project-root-name 'face 'cb-header-line-format-project-name)
              (cb-header-line-format--nonemphasised ") ")))
     (t
      ""))))

(defun cb-header-line-format--host-info ()
  (concat
   (cb-header-line-format--nonemphasised " (at ")
   (propertize (and (boundp 'tramp-current-host) tramp-current-host) 'face 'cb-header-line-format-host-name)
   (cb-header-line-format--nonemphasised ") ")))

(defun cb-header-line-format--context-info ()
  (cond
   ((not (cb-header-line-format--window-selected?))
    "")
   ((file-remote-p default-directory)
    "")
   (t
    (cb-header-line-format--project-info))))

(defun cb-header-line-format--major-mode-icon ()
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

(defun cb-header-line-format--major-mode-info ()
  (if (cb-header-line-format--window-selected?)
      (cb-header-line-format--major-mode-icon)
    "   "))

(defun cb-header-line-format--buffer-name ()
  (if (cb-header-line-format--window-selected?)
      (buffer-name)
    (propertize (buffer-name) 'face 'cb-header-line-format-nonemphasised-element)))

(defun cb-header-line-format--line-info ()
  (let ((str "%2l"))
    (if (cb-header-line-format--window-selected?)
        str
      (propertize str 'face 'cb-header-line-format-nonemphasised-element))))

(defconst cb-header-line-format--flycheck-icon (all-the-icons-material "error_outline"))
(defconst cb-header-line-format--tree-icon (all-the-icons-octicon "file-directory" :v-adjust 0.05))
(defconst cb-header-line-format--ilist-icon (all-the-icons-fileicon "api-blueprint" :v-adjust 0.05))
(defconst cb-header-line-format--org-icon (all-the-icons-icon-for-mode 'org-mode :v-adjust 0.05))

(defconst cb-header-line-format
  '((:eval (pcase (buffer-name)
             ;; Special buffers
             ((or "*Help*" (guard (string-match-p (rx bos "*helpful ") (buffer-name))))
              (concat " " (cb-header-line-format--major-mode-icon) " Help"))
             ("*Flycheck errors*"
              (concat " " cb-header-line-format--flycheck-icon " Errors"))
             ("*Org Select*"
              (concat " " cb-header-line-format--org-icon " *Org Select*"))
             ("*compilation*"
              (concat " " (cb-header-line-format--major-mode-icon) " Compile"))
             ((guard (string-match-p (rx "*Treemacs-Framebuffer-") (buffer-name)))
              (concat " " cb-header-line-format--tree-icon " Filesystem"))
             ("*Ilist*"
              (concat " " cb-header-line-format--ilist-icon " Definitions"))
             ((guard (string-match-p (rx "-popup*" eos) (buffer-name)))
              (concat " " (cb-header-line-format--major-mode-icon) (buffer-name)))

             (_
              '(
                ;; Print error on low memory
                "%e"
                " "

                ;; Emacsclient info
                mode-line-client

                ;; Major mode icon
                (:eval (cb-header-line-format--major-mode-info))

                ;; Current line, padded
                (:eval (cb-header-line-format--line-info))
                "  "
                (:propertize "%6p " face cb-header-line-format-nonemphasised-element)

                ;; Modification indicator.
                (:eval (cb-header-line-format--access-mode-info))

                ;; Buffer name, with braces on recursive edit
                "  %[" (:eval (cb-header-line-format--buffer-name)) "%] "

                (:eval (cb-header-line-format--narrowing-info))

                (:eval (cb-header-line-format--context-info))))))))

(provide 'cb-header-line-format)

;;; cb-header-line-format.el ends here
