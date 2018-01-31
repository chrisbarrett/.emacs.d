;;; cb-emacs.el --- Variables relating to core Emacs functionality. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(autoload 'git-subtree-add "git-subtree" nil t)
(autoload 'git-subtree-push "git-subtree" nil t)
(autoload 'git-subtree-update "git-subtree" nil t)

(defvar magit-process-raise-error)

(defun cb-emacs-pinned-subtrees (subtree-path)
  (pcase subtree-path
    ("lisp/ensime-emacs" "v2.0.1")))

;; Config Paths

(defconst cb-emacs-cache-directory
  (concat user-emacs-directory ".cache"))

(defconst cb-emacs-autosave-directory
  (concat user-emacs-directory "autosave"))

(defconst cb-emacs-lisp-directory
  (concat user-emacs-directory "lisp"))

(defconst cb-emacs-elpa-directory
  (concat user-emacs-directory "elpa"))

(defconst cb-emacs-config-directory
  (concat user-emacs-directory "config"))

(defconst cb-emacs-site-lisp-directory
  (file-truename "~/.nix-profile/share/emacs/site-lisp"))

(add-to-list 'load-path cb-emacs-site-lisp-directory)

;; Commands for working with config subtrees

(defalias 'emacs-update-subtree #'cb-emacs-update-subtree)
(defalias 'emacs-add-subtree #'cb-emacs-add-subtree)
(defalias 'emacs-push-subtree #'cb-emacs-push-subtree)

;; Declare dynamic vars to satisfy byte compiler.
(defvar git-subtree-prefix nil)
(defvar git-subtree-subtree-to-rev-function nil)

(defun cb-emacs-add-subtree ()
  "Add a new subtree to .emacs.d/lisp."
  (interactive)
  (let* ((default-directory user-emacs-directory)
         (git-subtree-prefix "lisp")
         (git-subtree-subtree-to-rev-function #'cb-emacs-pinned-subtrees)
         (subtree-fullpath (call-interactively #'git-subtree-add)))
    (message "Recompiling lisp files...")
    (byte-recompile-directory subtree-fullpath 0)
    (cb-init/init-load-path)
    (message "Finished.")))

(defun cb-emacs-update-subtree ()
  "Update an existing subtree in .emacs.d/lisp."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (let* ((git-subtree-prefix "lisp")
           (git-subtree-subtree-to-rev-function #'cb-emacs-pinned-subtrees)
           (subtree-fullpath (call-interactively #'git-subtree-update)))
      (message "Recompiling lisp files...")
      (ignore-errors
        (let ((elc-files (f-files subtree-fullpath (lambda (f) (f-ext? f "elc")) t)))
          (-each elc-files #'f-delete)))
      (byte-recompile-directory subtree-fullpath 0)
      (message "Finished."))))

(defun cb-emacs-push-subtree ()
  "Push existing subtree in .emacs.d/lisp."
  (interactive)
  (let* ((default-directory user-emacs-directory)
         (git-subtree-prefix "lisp"))
    (call-interactively #'git-subtree-push)))

(defun cb-emacs-compile-subtree (subtree)
  "Force the byte compilation of SUBTREE."
  (interactive (list
                (completing-read "Select subtree to byte-recompile: "
                                 (-map #'f-filename (f-directories cb-emacs-lisp-directory))
                                 t)))
  (byte-recompile-directory (f-join cb-emacs-lisp-directory subtree) 0 t))

(defun cb-emacs-compile-all-subtrees ()
  "Force the byte compilation all subtrees."
  (interactive)
  (byte-recompile-directory cb-emacs-lisp-directory 0 t))

(defun cb-emacs-compile-elpa ()
  "Force the byte compilation ELPA directory."
  (interactive)
  (byte-recompile-directory cb-emacs-elpa-directory 0 t))

(provide 'cb-emacs)

;;; cb-emacs.el ends here
