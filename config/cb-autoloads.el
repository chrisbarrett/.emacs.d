;;; cb-autoloads.el --- <enter description here>  -*- lexical-binding: t; -*-
;;; Commentary:

;; I slam all package autoloads into a single file and read them in here.

;;; Code:

(require 'use-package)

(autoload 'f-files "f")
(autoload 'end-of-thing "thingatpt")

(defcustom cb-autoloads-file (expand-file-name "autoloads.el" user-emacs-directory)
  "Path to the autoloads file that will be generated."
  :group 'cb-autoloads
  :type 'file)



(defvar cb-autoloads-file (expand-file-name "autoloads.el" user-emacs-directory))

(defconst cb-autoloads--load-path-update-form
  '(add-to-list 'load-path (or (and load-file-name
                                    (file-name-directory load-file-name))
                               (car load-path))))

(defun cb-autoloads--dep-files ()
  (seq-mapcat (lambda (dir)
                (when (file-directory-p dir)
                  (unless (or (file-equal-p dir user-emacs-directory)
                              (file-equal-p dir (expand-file-name "config" user-emacs-directory)))
                    (directory-files dir t (rx "-autoloads.el" eos)))))
              load-path))

(defun cb-autoloads-build-and-load ()
  "Generate a consolidated autoloads file of all installed packages."
  (interactive)
  (unless (file-exists-p cb-autoloads-file)
    (with-current-buffer (find-file-noselect cb-autoloads-file t)
      (let ((inhibit-read-only t))

        (erase-buffer)
        (insert (format ";;; %s -- All package autoloads slammed into one place.  -*- lexical-binding: t; buffer-read-only: t; -*-\n"
                        (file-name-nondirectory cb-autoloads-file)))
        (insert "\n")

        (dolist (autoloads-file (cb-autoloads--dep-files))
          (newline 2)
          (insert (string-trim (with-temp-buffer
                                 (insert-file-contents autoloads-file)
                                 (goto-char (point-min))
                                 (when (search-forward-regexp (rx "(add-to-list" (+ space) "'load-path" symbol-end) nil t)
                                   (goto-char (match-beginning 0))
                                   (let ((form (read (thing-at-point 'sexp))))
                                     (when (equal form cb-autoloads--load-path-update-form)
                                       (delete-region (point) (end-of-thing 'sexp))
                                       (insert (prin1-to-string `(add-to-list 'load-path ,(file-name-directory autoloads-file)))))

                                     ;; Fix weird issue loading proof-general due to how it tries to compute its root dir.
                                     (goto-char (point-min))
                                     (when (search-forward "(expand-file-name \"generic/proof-site\" pg-init--pg-root)" nil t)
                                       (delete-region (match-beginning 0) (match-end 0)))))

                                 (buffer-string)))))
        (save-buffer))))
  (load cb-autoloads-file t))

(cb-autoloads-build-and-load)

(provide 'cb-autoloads)

;;; cb-autoloads.el ends here
