;;; paths.el --- Path variables and path management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'f)
(require 'subr-x)
(require 'seq)

(autoload 'hostname (concat user-emacs-directory "lisp/hostname"))

(defvar org-directory "~/org")



(defconst paths-assets-directory
  (concat user-emacs-directory "assets"))

(defconst paths-cache-directory
  (concat user-emacs-directory "var"))

(defconst paths-etc-directory
  (concat user-emacs-directory "etc"))

(defconst paths-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst paths-elpa-directory
  (concat user-emacs-directory "elpa"))

(defconst paths-config-directory
  (concat user-emacs-directory "config"))

(defconst paths-hacks-directory
  (concat user-emacs-directory "hacks"))

(defconst paths-themes-directory
  (concat user-emacs-directory "themes"))

(defconst paths-site-lisp-directory
  (seq-find #'file-directory-p
            '("/run/current-system/sw/share/emacs/site-lisp"
              "~/.nix-profile/share/emacs/site-lisp")))

(defconst paths-hostfile
  (format "~/gdrive/personal-config/hostfile-%s.el" (hostname)))

(defconst paths-personal-config
  "~/gdrive/personal-config/personal-config.el")

(defconst paths-project-directories
  (seq-filter #'file-directory-p '("~/Documents"
                                   "~/Projects"
                                   "~/workspace"
                                   "~/.local/src")))



(defun paths-initialise (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (main-dirs
          (list paths-lisp-directory
                paths-config-directory
                paths-themes-directory
                paths-hacks-directory
                paths-site-lisp-directory))
         (subdirs
          (f-directories paths-lisp-directory))
         (updated-load-path
          (seq-filter (lambda (it) (and it (file-directory-p it)))
                      (seq-uniq (append main-dirs subdirs load-path)))))

    (setq load-path updated-load-path)

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))


(provide 'paths)

;;; paths.el ends here
