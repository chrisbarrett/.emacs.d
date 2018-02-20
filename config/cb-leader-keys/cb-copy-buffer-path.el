;;; cb-copy-buffer-path.el --- Copy the path to the visited file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'subr-x)

(defun cb/copy-buffer-path ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let ((path (or (buffer-file-name) list-buffers-directory)))
      (message (kill-new path))
    (error "Buffer not visiting a file")))

(defun cb/copy-buffer-name ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((name (if-let ((path (buffer-file-name)))
                  (file-name-nondirectory path)
                (buffer-name))))
    (message (kill-new name))))

(defun cb/copy-buffer-directory ()
  "Show and copy the directory of the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let ((path (or (ignore-errors (file-name-directory (buffer-file-name))) list-buffers-directory)))
      (message (kill-new path))
    (error "Buffer not visiting a file")))

(provide 'cb-copy-buffer-path)

;;; cb-copy-buffer-path.el ends here
