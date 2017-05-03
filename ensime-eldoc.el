;;; ensime-eldoc.el --- ELDoc Support for ensime

;;; Commentary:

;;; Code:

(require 'eldoc)
(require 'ensime-client)
(require 'ensime-model)
(require 'ensime-notes)

(defun ensime-eldoc-info ()
  "ELDoc backend for ensime."
  ;; The response from `ensime-rpc-symbol-at-point' has the type info but,
  ;; its sligthly different from the one obtained with `ensime-type-at-point'
  ;; Using the underlying `ensime-rpc-get-type-at-point' to maintain consistency
  (when (ensime-connected-p)
    (let ((msg (pcase ensime-eldoc-hints
                 (`error
                  (ensime-errors-at (point)))
                 (`implicit
                  (ensime-implicit-notes-at (point)))
                 (`type
                  (ensime-eldoc-type-info))
                 (`all
                  (let* ((error-msg (ensime-errors-at (point)))
                         (implicit (ensime-implicit-notes-at (point)))
                         (type (ensime-eldoc-type-info)))
                    (format "%s\n%s\n%s"
                            type
                            (if implicit implicit "")
                            (if error-msg error-msg "")))))))
      (eldoc-message (s-trim msg)))))

(defun ensime-eldoc-type-info ()
  "Get type information at point in a format suitable for eldoc"
  (when (ensime-connected-p)
    (let* ((symbol (ensime-rpc-symbol-at-point))
           (type (ensime-rpc-get-type-at-point))
           (name (plist-get symbol :local-name))
           (type-name (ensime-type-name-with-args type)))
      (when (and type (not (string= type-name "<none>")))
        (format "%s: %s" name type-name)))))

(provide 'ensime-eldoc)

;;; ensime-eldoc.el ends here
