;;; git-subtree.el --- Commands for working with git subtrees  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; Package-Requires: ((magit "2.11.0") (emacs "25"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides commands for working with git subtrees.

;;; Code:

(require 'magit)
(require 'seq)
(require 'subr-x)

;; Dynamically-bindable variables

(defvar git-subtree-prefix nil
  "The directory in which subtrees will be created, relative to the git root.

Dynamically-bind this var before calling `git-subtree-add' to
prepopulate the parent directory for the subtree.")

(defvar git-subtree-subtree-to-rev-function nil
  "Function used to determine which rev to pull or update.

It takes a single argument, which is the relative path of the
subtree as a string. It is expected to return the git revision to
use, as a string.

Dynamically-bind this var before calling `git-subtree-add' to
prepopulate the rev.

If this var is bound when calling `git-subtree-update', and if it
returns non-nil, the return value will be used as the rev.")

;; Utilities

(defun git-subtree--assert-tree-not-dirty ()
  (when (magit-anything-modified-p)
    (user-error "`%s' has uncommitted changes.  Aborting" default-directory)))

(defun git-subtree--expand-file-name (path)
  (let (file-name-handler-alist)
    (directory-file-name (expand-file-name path))))

(defun git-subtree--list ()
  "Find subtrees by trawling through the git history.

See:

  https://stackoverflow.com/questions/16641057/how-can-i-list-the-git-subtrees-on-the-root"
  (split-string
   (shell-command-to-string
    "git log \
     | grep git-subtree-dir \
     | tr -d ' ' \
     | cut -d \":\" -f2 \
     | sort | uniq \
     | xargs -I {} bash -c 'if [ -d $(git rev-parse --show-toplevel)/{} ]; then echo {}; fi'")))

(defmacro git-subtree--with-signal-handlers (interactive-p desc &rest body)
  "Helper macro for working with magit errors.

If INTERACTIVE-P is non-nil, log DESC.

Any magit errors raised in BODY forms will be intercepted for
better user feedback."
  (declare (indent 2))
  `(condition-case _
       (let ((magit-process-raise-error t))
         (when ,interactive-p
           (message "%s" ,desc))
         ,@body)
     (magit-git-error
      (magit-process-buffer t)
      (error "%sfailed.  See the magit process buffer for details" ,desc))
     (error
      (error "%sfailed" ,desc))))

;; Remotes

(defun git-subtree--last-path-component (path)
  (car (last (split-string path "[/]" t))))

(defun git-subtree--guess-remotes (subtree-path remotes)
  (let ((suffix (git-subtree--last-path-component subtree-path)))
    (seq-filter (lambda (it)
                  (equal (git-subtree--last-path-component it) suffix))
                remotes)))

(defun git-subtree--default-name-for-url (url)
  (thread-first (file-name-sans-extension url)
    (split-string "[/]")
    (nreverse)
    (seq-take 2)
    (nreverse)
    (string-join "/")))

(defun git-subtree--read-new-remote ()
  (let* ((url (magit-read-url "Remote url"))
         (name (magit-read-string-ns "Remote name" (git-subtree--default-name-for-url url))))
    (unless (member name (magit-list-remotes))
      (git-subtree--with-signal-handlers t "Adding remote..."
        (magit-run-git "remote" "add" name url)))
    name))

(defun git-subtree--read-existing-remote (subtree-path)
  (let ((remotes (magit-list-remotes)))
    (pcase (git-subtree--guess-remotes subtree-path remotes)
      (`(,remote) remote)
      (`nil
       (completing-read "Remote: " remotes nil t))
      (guesses
       (completing-read "Remote: " guesses nil t)))))

;; User-facing functions

(defun git-subtree--subtree-path-for-remote (remote)
  (if git-subtree-prefix
      (expand-file-name (file-name-nondirectory remote) git-subtree-prefix)
    (file-name-nondirectory remote)))

(defun git-subtree--rev-for-subtree (subtree-path)
  (or (when git-subtree-subtree-to-rev-function
        (funcall git-subtree-subtree-to-rev-function subtree-path))
      "master"))

(defun git-subtree-add (subtree-path remote &optional rev interactive-p)
  "Add a new subtree.

SUBTREE-PATH is the path where the subtree will be pulled to,
relative to the root of the git repository.

REMOTE is the remote name. When called interactively, a remote
will be added at a given URL.

If REV is nil, the value defaults to \"master\".

If INTERACTIVE-P is set, messages will be logged indicating progress."
  (interactive  (let ((default-directory (magit-toplevel)))
                  (git-subtree--assert-tree-not-dirty)
                  (let* ((remote (git-subtree--read-new-remote))
                         (subtree-path (file-relative-name
                                        (read-file-name "Create at: " (git-subtree--subtree-path-for-remote remote)))))
                    (list subtree-path
                          remote
                          (read-string "Rev: " (git-subtree--rev-for-subtree subtree-path))
                          'interactive-p))))

  (let ((default-directory (magit-toplevel))
        (rev (or rev "master")))
    (git-subtree--assert-tree-not-dirty)
    (run-hooks 'magit-credential-hook)

    (let ((fullpath (git-subtree--expand-file-name subtree-path))
          (commit-message (format "Add %s@master to %s" remote subtree-path)))

      (when interactive-p
        (unless (y-or-n-p (format "%s at %s will be merged to %s. Continue? " remote rev fullpath))
          (user-error "Aborted")))

      (git-subtree--with-signal-handlers interactive-p "Fetching remote..."
        (magit-run-git "fetch" "-q" remote))

      (git-subtree--with-signal-handlers interactive-p "Importing subtree..."
        (magit-run-git "subtree" "-q" "add" "--prefix" subtree-path remote rev "--squash" "-m" commit-message))

      (when interactive-p
        (message "Subtree `%s' added successfully." subtree-path))

      fullpath)))

(defun git-subtree-update (subtree-path &optional remote rev interactive-p)
  "Update an existing subtree.

SUBTREE-PATH is the path to the subtree, relative to the git
root.

REMOTE is the git remote to fetch from.

REV is the git revision to update to.

If INTERACTIVE-P is set, messages will be logged indicating progress."
  (interactive  (let ((default-directory (magit-toplevel)))
                  (git-subtree--assert-tree-not-dirty)
                  (let ((subtree-path (completing-read "Select subtree to update: " (git-subtree--list) t)))
                    (list subtree-path
                          (git-subtree--read-existing-remote subtree-path)
                          (git-subtree--rev-for-subtree subtree-path)
                          'interactive))))

  (let ((default-directory (magit-toplevel)))
    (git-subtree--assert-tree-not-dirty)

    (let ((fullpath (git-subtree--expand-file-name subtree-path))
          (commit-message (format "Merge %s@%s into %s" remote rev subtree-path)))

      (when interactive-p
        (unless (y-or-n-p (format "%s at %s will be merged to %s. Continue? " remote rev fullpath))
          (user-error "Aborted")))

      (run-hooks 'magit-credential-hook)

      (git-subtree--with-signal-handlers interactive-p "Fetching remote..."
        (magit-run-git "fetch" "-q" remote))

      (git-subtree--with-signal-handlers interactive-p "Importing subtree..."
        (magit-run-git "subtree" "-q" "pull" "--prefix" subtree-path remote rev "--squash" "-m" commit-message))

      (when interactive-p
        (message "Subtree `%s' updated successfully." subtree-path))

      fullpath)))

(defun git-subtree-push (subtree-path &optional branch remote interactive-p)
  "Push a subtree.

SUBTREE-PATH is the path of the subtree, relative to the git
root.

BRANCH is the target branch on the remote repository.

REMOTE is the git remote to push to.

If INTERACTIVE-P is set, log extra progress information."
  (interactive  (let* ((default-directory user-emacs-directory)
                       (subtree-path (completing-read
                                      "Select subtree to update: "
                                      (git-subtree--list)
                                      t)))
                  (list subtree-path
                        (read-string "Branch: " "master")
                        (git-subtree--read-existing-remote subtree-path)
                        'interactive-p)))

  (let* ((default-directory (magit-toplevel))
         (fullpath (git-subtree--expand-file-name subtree-path)))

    (when interactive-p
      (unless (y-or-n-p (format "Commits under %s will be pushed to %s at %s. Continue? "
                                (abbreviate-file-name fullpath) remote branch))
        (user-error "Aborted")))

    (run-hooks 'magit-credential-hook)

    (git-subtree--with-signal-handlers interactive-p "Pushing subtree..."
      (magit-process-buffer)
      (magit-run-git-async "subtree" "push" "--prefix" subtree-path remote branch))
    fullpath))

;; cbf writing an ert test suite right now.

(cl-assert (equal (git-subtree--default-name-for-url "https://github.com/foo/bar.git") "foo/bar"))

(cl-assert (equal (git-subtree--guess-remotes "foo/bar/baz/" '("remote/bar" "remote/baz"))
                  '("remote/baz"))
           t)

(cl-assert (equal (git-subtree--guess-remotes "foo/bar/baz" '("remote/baz" "remote/bar"))
                  '("remote/baz"))
           t)

(cl-assert (equal (git-subtree--guess-remotes "baz" '("a/baz" "b/baz"))
                  '("a/baz" "b/baz"))
           t)

(provide 'git-subtree)

;;; git-subtree.el ends here
