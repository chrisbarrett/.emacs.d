;;; ledger-hacks.el --- Hacks for ledger-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(cl-eval-when (compile)
  (require 'ledger-report))

(el-patch-feature ledger-report)

(with-eval-after-load 'ledger-report
  (el-patch-defun ledger-do-report (cmd)
    "Run a report command line CMD.
CMD may contain a (shell-quoted) version of
`ledger-report--extra-args-marker', which will be replaced by
arguments returned by `ledger-report--compute-extra-args'."
    (goto-char (point-min))
    (let* ((inhibit-read-only t)
           (marker ledger-report--extra-args-marker)
           (marker-re (concat " *" (regexp-quote marker)))
           (args (ledger-report--compute-extra-args cmd))
           (args-str (concat " " (mapconcat #'shell-quote-argument args " ")))
           (clean-cmd (replace-regexp-in-string marker-re "" cmd t t))
           (real-cmd (replace-regexp-in-string marker-re args-str cmd t t)))
      (setq header-line-format
            (and ledger-report-use-header-line
                 `(:eval (ledger-report--compute-header-line ,clean-cmd))))
      (unless ledger-report-use-header-line
        (insert (format "Report: %s\n" ledger-report-name)
                (format "Command: %s\n" clean-cmd)

                (el-patch-swap (make-string (- (window-width) 1) ?=)
                               "")
                "\n\n"))
      (let* ((report (shell-command-to-string real-cmd)))
        (when ledger-report-use-native-highlighting
          (setq report (ansi-color-apply report)))
        (save-excursion
          (insert report))
        (when (ledger-report--cmd-needs-links-p cmd)
          (save-excursion
            (ledger-report--add-links))))

      (el-patch-add
        (page-break-lines--update-display-tables)))))

(provide 'ledger-hacks)

;;; ledger-hacks.el ends here
