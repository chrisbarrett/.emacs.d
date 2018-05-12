;;; cb-ledger-redefinitions.el --- Redefinitions for ledger functions.

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(autoload 'ledger-navigate-to-line "ledger-mode")


;; HACK: Hide command name from reports.
(defun ledger-do-report (cmd)
  (goto-char (point-min))
  (insert (format "Report: %s\n" ledger-report-name)
          (make-string (- (window-width) 1) ?=)
          "\n\n")
  (let ((data-pos (point))
        (register-report (string-match " reg\\(ister\\)? " cmd))
        files-in-report)
    (shell-command
     ;; --subtotal does not produce identifiable transactions, so don't
     ;; prepend location information for them
     cmd
     t nil)
    (when register-report
      (goto-char data-pos)
      (while (re-search-forward "^\\(/[^:]+\\)?:\\([0-9]+\\)?:" nil t)
        (let ((file (match-string 1))
              (line (string-to-number (match-string 2))))
          (delete-region (match-beginning 0) (match-end 0))
          (when file
            (set-text-properties (line-beginning-position) (line-end-position)
                                 (list 'ledger-source (cons file (save-window-excursion
                                                                   (save-excursion
                                                                     (find-file file)
                                                                     (widen)
                                                                     (ledger-navigate-to-line line)
                                                                     (point-marker))))))
            (add-text-properties (line-beginning-position) (line-end-position)
                                 (list 'font-lock-face 'ledger-font-report-clickable-face))
            (end-of-line)))))
    (goto-char data-pos)))

(provide 'cb-ledger-redefinitions)

;;; cb-ledger-redefinitions.el ends here
