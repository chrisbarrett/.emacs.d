;;; init.el --- Init file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:

;; Defines the machinery needed to load the Emacs configuration from config.org.

;;; Code:

(require 'cl-macs)

(cl-eval-when (compile)
  (require 'org))

(defvar tangle-debug nil
  "Whether to emit extra debugging information when tangling init files.")

(defun tangle-init-files ()
  "Create init files from `config.org'."
  (interactive)
  (let ((file-to-block-list))
    (message "Re-generating init files…")
    (save-restriction
      (save-excursion
        (require 'org)
        (let ((inhibit-message t))
          (org-babel-map-src-blocks "config.org"
            (when (equal lang "emacs-lisp")
              (cond ((member "disabled" (org-get-tags))
                     (when tangle-debug
                       (message "Init files: Skipping:\n%s" body)))
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
              (message "—————• Writing %s…" file)))))
      (message "Wrote all init files."))))

(let ((this-file (or load-file-name (buffer-file-name))))
  (unless (file-exists-p (expand-file-name "config.el" (file-name-directory this-file)))
    (tangle-init-files)))

(load-file "./config.el")

;; (provide 'init)

;;; init.el ends here
