;;; cb-emacs.el --- Variables relating to core Emacs functionality. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(autoload 'magit-anything-modified-p "magit-git")
(autoload 'magit-list-remotes "magit-git")
(autoload 'magit-process-buffer "magit-process")
(autoload 'magit-read-string-ns "magit-utils")
(autoload 'magit-read-url "magit-remote")
(autoload 'magit-run-git "magit-process")

(defvar magit-process-raise-error)

(defconst cb-emacs-pinned-subtree-versions
  '((ensime-emacs . "v2.0.1")))


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

(defalias 'update-subtree #'cb-emacs-update-subtree)
(defalias 'add-subtree #'cb-emacs-add-subtree)
(defalias 'push-subtree #'cb-emacs-push-subtree)

(defun cb-emacs--find-subtree-remote (subtree)
  (--find (equal (-last-item (s-split "/" it)) subtree)
          (magit-list-remotes)))

(defmacro cb-emacs--with-signal-handlers (step &rest body)
  (declare (indent 1))
  `(condition-case _
       (let ((magit-process-raise-error t))
         (message "%s" ,step)
         ,@body)
     (magit-git-error
      (error "%sfailed.  See %s" ,step (magit-process-buffer t)))
     (error
      (error "%sfailed" ,step ))))

(defun cb-emacs--read-new-remote ()
  (let* ((name (magit-read-string-ns "Remote name"))
         (url (magit-read-url "Remote url" (format "https://github.com/%s.git" name))))
    (unless (member name (magit-list-remotes))
      (cb-emacs--with-signal-handlers "Adding remote..."
        (magit-run-git "remote" "add" name url)))
    name))

(defun cb-emacs--assert-tree-not-dirty ()
  (require 'magit)
  (when (magit-anything-modified-p)
    (user-error "`%s' has uncommitted changes.  Aborting" default-directory)))

(defun cb-emacs-add-subtree (subtree remote version)
  "Add a new SUBTREE at REMOTE at VERSION."
  (interactive  (let ((default-directory user-emacs-directory))
                  (cb-emacs--assert-tree-not-dirty)
                  (let* ((remote (cb-emacs--read-new-remote))
                         (subtree (file-name-nondirectory remote))
                         (ref (read-string "Ref: " (alist-get (intern subtree) cb-emacs-pinned-subtree-versions "master"))))
                    (list subtree remote ref))))
  (let ((default-directory user-emacs-directory))
    (cb-emacs--assert-tree-not-dirty)
    (run-hooks 'magit-credential-hook)

    (let* ((prefix (format "lisp/%s" subtree))
           (fullpath (f-join cb-emacs-lisp-directory subtree))
           (commit-message (format "Add %s@master to %s" remote prefix)))

      (unless (y-or-n-p (format "%s at %s will be merged to %s. Continue? " remote version fullpath))
        (user-error "Aborted"))

      (cb-emacs--with-signal-handlers "Fetching remote..."
        (magit-run-git "fetch" "-q" remote))

      (cb-emacs--with-signal-handlers "Importing subtree..."
        (magit-run-git "subtree" "-q" "add" "--prefix" prefix remote version "--squash" "-m" commit-message))

      (cb-emacs--with-signal-handlers "Compiling..."
        (byte-recompile-directory fullpath 0))

      (cb-init/init-load-path)

      (message "Subtree `%s' added successfully." prefix))))

(defun cb-emacs-update-subtree (subtree &optional remote)
  "Update SUBTREE at REMOTE.

When called interactively, prompt for the subtree, then only
prompt for REMOTE if it cannot be determined."
  (interactive  (let ((default-directory user-emacs-directory))
                  (cb-emacs--assert-tree-not-dirty)
                  (let ((subtree (completing-read
                                  "Select subtree to update: "
                                  (-map #'f-filename (f-directories cb-emacs-lisp-directory))
                                  t)))
                    (list subtree
                          (or (cb-emacs--find-subtree-remote subtree)
                              (cb-emacs--read-new-remote))))))

  (let ((default-directory user-emacs-directory))
    (cb-emacs--assert-tree-not-dirty)

    (let* ((prefix (format "lisp/%s" subtree))
           (fullpath (f-join cb-emacs-lisp-directory subtree))
           (version (alist-get (intern subtree) cb-emacs-pinned-subtree-versions "master"))
           (commit-message (format "Merge %s@%s into %s" remote version prefix)))

      (unless (y-or-n-p (format "%s at %s will be merged to %s. Continue? " remote version fullpath))
        (user-error "Aborted"))

      (run-hooks 'magit-credential-hook)

      (cb-emacs--with-signal-handlers "Fetching remote..."
        (magit-run-git "fetch" "-q" remote))

      (cb-emacs--with-signal-handlers "Importing subtree..."
        (magit-run-git "subtree" "-q" "pull" "--prefix" prefix remote version "--squash" "-m" commit-message))

      (cb-emacs--with-signal-handlers "Removing existing byte-compiled files..."
        (ignore-errors
          (let ((elc-files (f-files fullpath (lambda (f) (f-ext? f "elc")) t)))
            (-each elc-files #'f-delete))))

      (cb-emacs--with-signal-handlers "Compiling..."
        (byte-recompile-directory fullpath 0))

      (message "Subtree `%s' updated successfully." prefix))))

(defun cb-emacs-push-subtree (subtree &optional branch remote)
  "Push to the SUBTREE to BRANCH at REMOTE.

When called interactively, prompt for the subtree, then only
prompt for REMOTE if it cannot be determined."
  (interactive  (let* ((default-directory user-emacs-directory)
                       (subtree (completing-read
                                 "Select subtree to update: "
                                 (-map #'f-filename (f-directories cb-emacs-lisp-directory))
                                 t)))
                  (list subtree
                        (read-string "Branch: " "master")
                        (or (cb-emacs--find-subtree-remote subtree)
                            (cb-emacs--read-new-remote)))))
  (let* ((default-directory user-emacs-directory)
         (prefix (format "lisp/%s" subtree))
         (fullpath (f-join cb-emacs-lisp-directory subtree)))

    (unless (y-or-n-p (format "Commits under %s will be pushed to %s at %s. Continue? "
                              (f-short fullpath) remote branch))
      (user-error "Aborted"))

    (run-hooks 'magit-credential-hook)

    (cb-emacs--with-signal-handlers "Pushing subtree..."
      (magit-process-buffer)
      (magit-run-git-async "subtree" "push" "--prefix" prefix remote branch))))

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
