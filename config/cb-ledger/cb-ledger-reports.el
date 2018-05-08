;;; cb-ledger-reports.el --- Utilities for generated ledger reports. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'ledger-mode)
(require 's)
(require 'subr-x)

(defvar cb-ledger-reports-income-payee-name nil
  "The account name for incoming funds.")

(defun cb-ledger-reports-last-n-pay-dates (n)
  (let ((output (shell-command-to-string
                 (format "ledger -f '%s' reg '%s' -y '%%Y-%%m-%%d' | tail -n %s | awk '{print $1}'"
                         ledger-master-file
                         cb-ledger-reports-income-payee-name
                         n))))
    (s-split "\n" output t)))

(defun cb-ledger-reports-previous-pay-period ()
  (-if-let ((prev cur) (cb-ledger-reports-last-n-pay-dates 2))
      (format "from %s to %s" prev (cb-ledger-reports--day-before cur))
    ;; Return empty results in degenerate case, where there is not at least one
    ;; month of records.
    "from 1970-01-01 to 1970-01-01"))

(defun cb-ledger-reports--day-before (ledger-date-str)
  (-let* (((y m d) (-map 'string-to-number (s-split "[/-]" ledger-date-str)))
          (date (encode-time 0 0 0 d m y))
          (updated (time-subtract date (days-to-time 1))))
    (format-time-string "%Y-%m-%d" updated)))

(provide 'cb-ledger-reports)

;;; cb-ledger-reports.el ends here
