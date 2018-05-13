;;; paths.el --- Path variables and path management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'recentf)
  (require 'use-package))

(require 'f)
(require 'subr-x)
(require 'seq)



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

(defconst paths-site-lisp-directory "~/.nix-profile/share/emacs/site-lisp")



;; no-littering overrides many common paths to keep the .emacs.d directory
;; clean.
;;
;; Load it here since we want to refer to path vars, and need to make sure it's
;; loaded very early in the startup process.

(use-package no-littering
  :straight t
  :demand t
  :init
  (progn
    (setq no-littering-etc-directory paths-etc-directory)
    (setq no-littering-var-directory paths-cache-directory))
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (add-to-list 'recentf-exclude no-littering-var-directory)))



(defun paths-initialise (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (main-dirs
          (list paths-lisp-directory
                paths-config-directory
                paths-themes-directory
                paths-site-lisp-directory))
         (subdirs
          (f-directories paths-lisp-directory))
         (updated-load-path
          (seq-filter #'file-directory-p (seq-uniq (append main-dirs subdirs load-path)))))

    (setq load-path updated-load-path)

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))


(provide 'paths)

;;; paths.el ends here
