;;; cb-delete-current-buffer-and-file.el --- Command to delete the current buffer and file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defvar cb-delete-current-buffer-and-file-file-deleted-functions nil
  "Hook called after a file is deleted by `cb-delete-current-buffer-and-file'.
Each function is passed the path of the file that was deleted.")

;;;###autoload
(defun cb/delete-current-buffer-and-file ()
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
      (run-hook-with-args cb-delete-current-buffer-and-file-file-deleted-functions file)
      (message "File deleted: %s" file)))))


(provide 'cb-delete-current-buffer-and-file)

;;; cb-delete-current-buffer-and-file.el ends here
