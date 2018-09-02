;;; cb-ledger-format.el --- Buffer formatting commands for ledger files.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(autoload 'ledger-mode-clean-buffer "ledger-mode")

(defvar ledger-post-amount-alignment-column 52)

(defun cb-ledger-format--align-price-assertion ()
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (when (s-matches? (rx "=" (* space) (not (any digit)))
                      line)
      (unwind-protect
          (progn
            (goto-char (line-beginning-position))
            (search-forward "=")
            (goto-char (match-beginning 0))
            (indent-to (1+ ledger-post-amount-alignment-column))
            (skip-chars-forward " =")
            (just-one-space))
        (goto-char (line-end-position))))))

;;;###autoload
(defun cb-ledger-format-buffer ()
  "Reformat the buffer."
  (interactive "*")
  (let ((pos (point)))
    (ledger-mode-clean-buffer)
    (goto-char (point-min))
    (while (search-forward "=" nil t)
      (cb-ledger-format--align-price-assertion))
    (goto-char pos)))

(provide 'cb-ledger-format)

;;; cb-ledger-format.el ends here
