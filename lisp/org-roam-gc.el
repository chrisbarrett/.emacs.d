;;; org-roam-gc.el --- Clean up empty roam files -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'org-element)

(defconst org-roam-gc-prompt-before-deleting-p nil
  "Whether to prompt before removing files when run interactively.")

(defconst org-roam-gc-debug nil
  "Whether to output extra messages for debugging purposes.")

(defun org-roam-gc--empty-content-p (buf)
  (with-current-buffer buf
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (equal 'property-drawer (org-element-type (org-element-at-point)))
          (org-forward-element))
        (while (and (equal 'keyword (org-element-type (org-element-at-point)))
                    (ignore-errors
                      (org-forward-element)
                      t)))
        (or (eobp)
            (string-blank-p (buffer-substring (1+ (point)) (point-max))))))))

(defun org-roam-gc--empty-file-content-p (file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-roam-gc--empty-content-p (current-buffer))))

(defun org-roam-gc-dailies-files ()
  (require 'org-roam)
  (require 'org-roam-dailies)
  (f-files (expand-file-name org-roam-dailies-directory org-roam-directory)))

(defmacro org-roam-gc--log (msg &rest args)
  `(when org-roam-gc-debug
     (message ,msg ,@args)
     nil))

(defun org-roam-gc--file-editing-p (file)
  (when-let* ((buf (find-buffer-visiting file)))
    (or (buffer-modified-p buf)
        (get-buffer-window-list buf))))

(defun org-roam-gc--remove-file (file confirm-p)
  (let ((file (expand-file-name file)))
    (cond
     ((org-roam-gc--file-editing-p file)
      (org-roam-gc--log "Skipping open file: %s" file))

     (t
      (org-roam-gc--log "Removing file: %s" file)
      (with-current-buffer (find-file file)
        (when (or (not confirm-p)
                  (y-or-n-p (format "Delete file `%s'? " (f-abbrev file))))
          (kill-buffer)
          (delete-file file)))
      t))))

(defun org-roam-gc (&optional interactive)
  "Delete empty org-roam dailies.

Optional arg INTERACTIVE determines whether to query before
removing files."
  (interactive "p")
  (let ((count
         (thread-last (org-roam-gc-dailies-files)
                      (seq-filter #'org-roam-gc--empty-file-content-p)
                      (seq-filter (lambda (file)
                                    (org-roam-gc--remove-file file (and interactive
                                                                        org-roam-gc-prompt-before-deleting-p))))
                      (length))))
    (cond
     (interactive
      (message "Deleted %s file%s" count (if (eq 1 count) "" "s")))
     ((< 0 count)
      (message "org-roam-gc deleted %s file%s" count (if (eq 1 count) "" "s"))))))

(provide 'org-roam-gc)

;;; org-roam-gc.el ends here
