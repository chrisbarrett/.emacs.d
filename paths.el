;;; paths.el --- Path variables and path management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'subr-x)
(require 'seq)

(defvar paths-org-directory "~/org")

(defconst paths-nix-directory "~/.config/nixpkgs")

(defconst paths-org-lisp-directory
  (expand-file-name "lisp" paths-org-directory))



(defconst paths-assets-directory
  (concat user-emacs-directory "assets"))

(defconst paths-cache-directory
  (expand-file-name "~/.cache/emacs"))

(unless (file-directory-p paths-cache-directory)
  (mkdir paths-cache-directory t))

(defconst paths-etc-directory
  (concat user-emacs-directory "etc"))

(defconst paths-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst paths-elpa-directory
  (concat user-emacs-directory "elpa"))

(defconst paths-site-lisp-directory
  (seq-find #'file-directory-p
            '("/run/current-system/sw/share/emacs/site-lisp"
              "~/.nix-profile/share/emacs/site-lisp")))

(defconst paths-hostfile
  (f-join user-emacs-directory "host.el"))

(defconst paths-project-directories
  (seq-filter (-compose #'file-directory-p #'car)
              '(("~/.config" . 1)
                ("~/.config/emacs/lisp" . 1)
                ("~/org/lisp" . 1)
                ("~/src" . 2))))



(defun paths-initialise (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (main-dirs (list paths-lisp-directory
                          (expand-file-name "nursery/lisp" paths-lisp-directory)
                          paths-site-lisp-directory))
         (subdirs (f-directories paths-lisp-directory))
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
