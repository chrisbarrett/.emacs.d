;;; buffer-cmds.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'subr-x)

(autoload 'projectile-invalidate-cache "projectile")
(autoload 'projectile-project-p "projectile")
(autoload 'recentf-cleanup "recentf")



;;;###autoload
(defun alternate-buffer (&optional window)
  "Toggle back and forth between two buffers.

WINDOW sets the window in which to toggle, and defaults to the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (car (seq-filter (lambda (buffer)
                            (and (not (eq buffer current-buffer))
                                 (or (null buffer-predicate) (funcall buffer-predicate buffer))))
                          (seq-map #'car (window-prev-buffers window))))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))



;;;###autoload
(defun copy-buffer-path ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let* ((path (or (buffer-file-name) list-buffers-directory)))
      (message (kill-new path))
    (error "Buffer not visiting a file")))

;;;###autoload
(defun copy-buffer-name ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((name (if-let* ((path (buffer-file-name)))
                  (file-name-nondirectory path)
                (buffer-name))))
    (message (kill-new name))))

;;;###autoload
(defun copy-buffer-directory ()
  "Show and copy the directory of the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let* ((path (or (ignore-errors (file-name-directory (buffer-file-name))) list-buffers-directory)))
      (message (kill-new path))
    (error "Buffer not visiting a file")))



;;;###autoload
(defun delete-current-buffer-and-file ()
  "Remove the file associated with the current buffer, then kill it."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((null file)
      (kill-buffer))
     ((not (file-exists-p file))
      (kill-buffer))
     ((yes-or-no-p "Delete this file? ")
      (delete-file file t)
      (kill-buffer)

      (when (and (featurep 'projectile) (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache))

      (message "File deleted: %s" file)))))



(defun buffer-cmds--assert-file-exists-for-buffer (&optional buf)
  (let ((cur (buffer-file-name buf)))
    (if (not (and cur (f-exists? cur)))
        (error "Buffer is not visiting a file!")
      cur)))

(defun buffer-cmds--try-move-file-with-vc (src dest)
  (condition-case err
      (when (vc-backend src)
        (vc-rename-file src dest)
        t)
    (error
     (let ((msg (error-message-string err)))
       (cond
        ((s-matches? "New file already exists" msg) nil)
        ((s-matches? "Please update files" msg)
         (unless (y-or-n-p "VC cannot track this change automatically.  Continue? ")
           (error msg)))
        (t
         (error msg)))))))

(defun buffer-cmds--try-rename-file (src dest)
  (when (and (f-exists? dest) (not (y-or-n-p "File exists.  Overwrite? ")))
    (user-error "Aborted"))
  (rename-file src dest t)
  (-when-let (buf (get-file-buffer src))
    (with-current-buffer buf
      (rename-buffer dest)
      (set-visited-file-name dest)
      (set-buffer-modified-p nil))

    (recentf-cleanup)
    (when (projectile-project-p)
      (projectile-invalidate-cache nil))))

;;;###autoload
(defun rename-file-and-buffer (buffer dest-dir dest-filename)
  "Rename the current buffer and file it is visiting.
Performs basic VC cleanup.

BUFFER is the buffer to rename.

DEST-DIR is the directory to move the underlying file to.

DEST-FILENAME is the new filename for the underlying file."
  (interactive (let ((cur (buffer-cmds--assert-file-exists-for-buffer)))
                 (list (current-buffer)
                       (read-directory-name "Move to directory: " (f-dirname cur))
                       (read-string "New name: " (f-filename cur)))))
  (let ((src (buffer-cmds--assert-file-exists-for-buffer buffer))
        (dest-path (f-join dest-dir dest-filename)))
    (or (buffer-cmds--try-move-file-with-vc src dest-path)
        (buffer-cmds--try-rename-file src dest-path))
    (when (and (fboundp 'projectile-project-p) (projectile-project-p))
      (call-interactively #'projectile-invalidate-cache))
    (message "File '%s' moved to '%s'" (f-short (f-filename src)) (f-short dest-path))))



(provide 'buffer-cmds)

;;; buffer-cmds.el ends here
