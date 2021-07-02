;;; paths.el --- Path variables and path management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'subr-x)
(require 'seq)

(defconst paths-ledger-directory "~/Documents/ledger")
(defconst paths-ledger-lisp-directory "~/Documents/ledger/lisp")

(defvar paths-org-directory "~/Documents/org")

(defconst paths-nix-directory "~/.config/nixpkgs")

(defconst paths-org-lisp-directory
  (expand-file-name "lisp" paths-org-directory))



(defconst paths-assets-directory
  (concat user-emacs-directory "assets"))

(defconst paths-cache-directory
  (expand-file-name "~/.cache/emacs"))

(unless (file-directory-p paths-cache-directory)
  (mkdir paths-cache-directory t))

(defconst paths-nursery-directory
  (concat user-emacs-directory "nursery"))

(defconst paths-etc-directory
  (concat user-emacs-directory "etc"))

(defconst paths-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst paths-elpa-directory
  (concat user-emacs-directory "elpa"))

(defconst paths-themes-directory
  (concat user-emacs-directory "themes"))

(defconst paths-site-lisp-directory
  (seq-find #'file-directory-p
            '("/run/current-system/sw/share/emacs/site-lisp"
              "~/.nix-profile/share/emacs/site-lisp")))

(defconst paths-hostfile
  (f-join user-emacs-directory "host.el"))

(defconst paths-personal-config
  "~/Dropbox/lisp/personal-config.el")

(defconst paths-project-directories
  (seq-filter #'file-directory-p '("~/Documents"
                                   "~/Projects"
                                   "~/.local/src")))



(defun paths-initialise (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (main-dirs
          (list paths-lisp-directory
                paths-themes-directory
                paths-site-lisp-directory))
         (subdirs
          (f-directories paths-lisp-directory))
         (updated-load-path
          (thread-last (append main-dirs subdirs load-path)
            (seq-uniq)
            (seq-filter (lambda (it) (and it (file-directory-p it)
                                          (not (f-same-p it user-emacs-directory))))))))
    (setq load-path updated-load-path)

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))


(provide 'paths)

;;; paths.el ends here
