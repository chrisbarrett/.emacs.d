;;; cb-autoloads.el --- <enter description here>  -*- lexical-binding: t; -*-
;;; Commentary:

;; I slam all package autoloads into a single file and read them in here.

;;; Code:

(require 'no-littering)

(autoload 'end-of-thing "thingatpt")

(defcustom cb-autoloads-file (no-littering-expand-var-file-name "autoloads/autoloads.el")
  "Path to the autoloads file that will be generated."
  :group 'cb-autoloads
  :type 'file)



(defconst cb-autoloads--load-file-reference
  '(and load-file-name (file-name-directory load-file-name))
  "Lisp form present in autoloads files used to update `load-path'.

This form needs to be replaced with the path to the package in
the Nix Store.")

(defun cb-autoloads--dep-files ()
  (seq-mapcat (lambda (dir)
                (when (file-directory-p dir)
                  (unless (or (file-equal-p dir user-emacs-directory)
                              (file-equal-p dir (expand-file-name "config" user-emacs-directory)))
                    (directory-files dir t (rx "-autoloads.el" eos)))))
              load-path))

(defun cb-autoloads--transform-autoloads-file (autoloads-file)
  (with-temp-buffer
    (insert-file-contents autoloads-file)
    (goto-char (point-min))
    (when (search-forward-regexp (rx "(and" (+ space) "load-file-name" symbol-end) nil t)
      (goto-char (match-beginning 0))
      (let ((form (read (thing-at-point 'sexp))))
        (when (equal form cb-autoloads--load-file-reference)
          (delete-region (point) (end-of-thing 'sexp))
          (insert (prin1-to-string (file-name-directory autoloads-file))))

        ;; Fix weird issue loading proof-general due to how it tries to compute its root dir.
        (goto-char (point-min))
        (when (search-forward "(expand-file-name \"generic/proof-site\" pg-init--pg-root)" nil t)
          (delete-region (match-beginning 0) (match-end 0)))))

    (buffer-string)))

(defun cb-autoloads-build-and-load (&optional rebuild)
  "Generate a consolidated autoloads file of all installed packages.

With optional argument REBUILD, unconditionally rebuild the file."
  (interactive (list t))
  (let ((autoloads-cache-dir (file-name-directory cb-autoloads-file)))
    (make-directory autoloads-cache-dir t)

    (when (or rebuild (not (file-exists-p cb-autoloads-file)))
      (with-current-buffer (find-file-noselect cb-autoloads-file t)
        (let* ((inhibit-read-only t)
               (page-break (char-to-string 12))
               (file-header (string-join (list ";;;"
                                               (file-name-nondirectory cb-autoloads-file)
                                               "--"
                                               "All package autoloads slammed into one place."
                                               "-*-"
                                               "lexical-binding: t;"
                                               "buffer-read-only: t;"
                                               "-*-")
                                         " ")))
          (erase-buffer)
          (insert file-header)
          (newline)
          (insert page-break)
          (newline)
          (dolist (autoloads-file (cb-autoloads--dep-files))
            (newline 2)
            (insert (string-trim (cb-autoloads--transform-autoloads-file autoloads-file))))
          (newline 2)
          (insert page-break)
          (insert "(provide 'autoloads)")
          (save-buffer))))

    ;; Add to load-path so config features can `require' it.
    (add-to-list 'load-path autoloads-cache-dir)
    (load cb-autoloads-file t)))

(provide 'cb-autoloads)

;;; cb-autoloads.el ends here
