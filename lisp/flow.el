;;; flow.el --- Utilities for working with Flow.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'json)
(require 's)

;;;###autoload
(defun flow-type-at (file line col)
  "Return the inferred type for FILE at the given LINE and COL.

When called interactively, print the type at point.
Otherwise return the parsed JSON response."
  (interactive
   (list
    (buffer-file-name)
    (line-number-at-pos)
    (current-column)))
  (let ((buffer (with-current-buffer (get-buffer-create " flow-type-at")
                  (erase-buffer)
                  (current-buffer))))

    (call-process "flow" nil buffer t
                  "type-at-pos"
                  file
                  (number-to-string line)
                  (number-to-string col)
                  "--json")
    (with-current-buffer buffer
      (-let [(parsed &as &alist 'type type) (json-read-from-string (buffer-string))]
        (if (called-interactively-p nil)
            (message "%s" type)
          parsed)))))

;;;###autoload
(defun flow-insert-flow-annotation ()
  "Insert a flow annotation at the start of this file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (s-matches? (rx (or (and "//" (* space) "@flow")
                            (and "/*" (* space) "@flow" (* space) "*/")))
                    (buffer-substring (line-beginning-position) (line-end-position)))
        (user-error "Buffer already contains an @flow annotation")
      (insert "// @flow\n")
      (message "Inserted @flow annotation."))))

(provide 'flow)

;;; flow.el ends here
