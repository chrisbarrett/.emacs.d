;;; init.el --- Init file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:

;; Defines the machinery needed to load the Emacs configuration from config.org.

;;; Code:

(defconst emacs-start-time (current-time)
  "The time at which this instance of Emacs was started.")

(require 'cl-macs)
(require 'f)
(require 'thingatpt)
(require 'subr-x)

(cl-eval-when (compile)
  (require 'org))

(defvar tangle-debug nil
  "Whether to emit extra debugging information when tangling init files.")

(defconst init--autoloads-file-header
  ";;; config-autoloads.el -- All package autoloads slammed into one place.  -*- lexical-binding: t; buffer-read-only: t; -*-")


(defun init--package-autoload-files ()
  (seq-mapcat (lambda (dir)
                (when (file-directory-p dir)
                  (f-files dir (lambda (file) (string-match-p "-autoloads.el$" file)))))
              load-path))

(defconst init--spurious-autoload-form
  '(add-to-list 'load-path (directory-file-name
                            (or (file-name-directory #$) (car load-path)))))

(defun init--clean-autoloads-file ()
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (and (search-forward-regexp (rx bol "(add-to-list 'load-path") nil t))
        (goto-char (line-beginning-position))
        (let ((form (read (thing-at-point 'sexp))))
          (when (equal form init--spurious-autoload-form)
            (delete-region (point) (end-of-thing 'sexp))))
        (goto-char (line-end-position)))

      ;; Fix weird issue loading proof-general due to how it tries to compute its root dir.
      (goto-char (point-min))
      (when (search-forward "(expand-file-name \"generic/proof-site\" pg-init--pg-root)" nil t)
        (delete-region (match-beginning 0) (match-end 0))))))

(defun generate-package-autoloads ()
  "Generate a consolidated autoloads file of all installed packages."
  (interactive)
  (let ((outfile (expand-file-name "config-autoloads.el")))

    (when-let* ((buf (get-buffer outfile)))
      (kill-buffer buf))
    (ignore-errors
      (f-delete outfile))

    (f-touch outfile)
    (f-append (concat init--autoloads-file-header "\n\n") 'utf-8 outfile)
    (let ((reporter (make-progress-reporter "Generating autoloads file...")))

      (dolist (autoloads-file (init--package-autoload-files))
        (let ((text (format ";;; %s\n%s\n\n" autoloads-file (f-read autoloads-file))))
          (f-append text 'utf-8 outfile)
          (progress-reporter-update reporter nil autoloads-file)))

      (with-current-buffer (find-file-noselect outfile t)
        (init--clean-autoloads-file)
        (save-buffer))

      (progress-reporter-done reporter))))

(defun tangle-init-files ()
  "Create init files from `config.org'."
  (interactive)
  (let ((file-to-block-list))
    (message "Re-generating init files…")
    (save-restriction
      (widen)
      (save-excursion
        (require 'org)
        (let ((inhibit-message t))
          (org-babel-map-src-blocks "config.org"
            (when (equal lang "emacs-lisp")
              (cond ((member "disabled" (org-get-tags))
                     (when tangle-debug
                       (message "Init files: Skipping:\n%s" body)))
                    ((member (org-get-todo-state) '("CANCELLED" "TODO"))
                     (when tangle-debug
                       (message "Init files: Skipping todo:\n%s" body)))
                    (t
                     (let* ((block-info (org-babel-get-src-block-info t))
                            (output-file (cdr (assq :tangle (nth 2 block-info)))))
                       (push (cons output-file body) file-to-block-list)
                       (when tangle-debug
                         (message "Init files: Adding:\n%s" body)))))))))
      (let ((lines-by-file
             (thread-last file-to-block-list
                          (seq-group-by #'car)
                          (seq-map (lambda (pair)
                                     (let ((lines (seq-reverse (seq-map #'cdr (cdr pair)))))
                                       (cons (car pair) (string-join lines "\n\n"))))))))
        (dolist (pair lines-by-file)
          (let ((file (car pair))
                (lines (cdr pair)))
            (with-temp-file file
              (insert lines)
              (indent-rigidly (point-min) (point-max) (- org-edit-src-content-indentation))
              (message "Writing %s…" file)))))

      (message "Wrote all init files."))))

(let ((this-file (or load-file-name (buffer-file-name))))

  (let ((config-autoloads-file (expand-file-name "config-autoloads.el" (file-name-directory this-file))))
    (unless (file-exists-p config-autoloads-file)
      (let ((default-directory (file-name-directory this-file)))
        (generate-package-autoloads))))

  (let ((config-lisp-file (expand-file-name "config.el" (file-name-directory this-file))))
    (unless (file-exists-p config-lisp-file)
      (let ((default-directory (file-name-directory this-file)))
        (tangle-init-files)))

    (unless (getenv "NIX_EMACS_BUILDING_CONFIG_P")
      (load config-lisp-file nil t))))


(defconst emacs-init-duration (float-time (time-subtract (current-time) emacs-start-time)))

(unless (getenv "NIX_EMACS_BUILDING_CONFIG_P")
  (message "config.el loaded (%s s)" emacs-init-duration))

;; (provide 'init)

;;; init.el ends here
