;;; cb-ledger-reports.el --- Utilities for generated ledger reports. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett
;; Package-Requires: ((emacs "25") (s "1.10.0") (ledger-mode "3.1.1") (dash "2.12.0"))
;; Author: Chris Barrett <chris+emacs@walrus.cool>
;; Version: 0.1

;;; Commentary:

;;; Code:

(require 'dash)
(require 'ledger-mode)
(require 's)

(require 'blergh)

(autoload 'org-fill-paragraph "org")

(defgroup cb-ledger-reports nil
  "Utilities for generating ledger reports."
  :group 'finance
  :prefix "cb-ledger-reports-")

(defcustom cb-ledger-reports-income-payee-name nil
  "The account name for incoming funds."
  :group 'cb-ledger-reports
  :type 'string)


;; Faces

(defface cb-ledger-reports-negative-amount
  `((t (:foreground "red")))
  "Face for negative amounts in ledger reports."
  :group 'ledger-faces)


;; Report State

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

(defvar cb-ledger-reports--current-state nil)

(defvar cb-ledger-reports--redraw-functions nil)


(defun cb-ledger-reports-state ()
  (append `((last-payday . ,(car (cb-ledger-reports-last-n-pay-dates 1)))
            (prev-pay-period . ,(cb-ledger-reports-previous-pay-period))
            (ledger-file . , ledger-master-file))
          cb-ledger-reports--current-state))

(defun cb-ledger-reports--state-update (action &optional args)
  (let ((updated (cb-ledger-reports--state-next cb-ledger-reports--current-state action args)))
    (setq cb-ledger-reports--current-state updated)
    (run-hook-with-args 'cb-ledger-reports--redraw-functions updated)))

(defun cb-ledger-reports--state-next (state action &optional args)
  (let ((next (copy-alist state)))
    (pcase action
      (:update-assets-and-liabilities
       (setf (alist-get 'assets-and-liabilities next) args))
      (:update-expenses-last-7-days
       (setf (alist-get 'expenses-last-7-days next) args))
      (:update-week-on-week-change
       (setf (alist-get 'week-on-week-change next) args))
      (:update-budget-last-7-days
       (setf (alist-get 'budget-last-7-days next) args))
      (:update-budget-last-30-days
       (setf (alist-get 'budget-last-30-days next) args))
      (:update-budget-since-payday
       (setf (alist-get 'budget-since-payday next) args))
      (:update-budget-last-pay-period
       (setf (alist-get 'budget-last-pay-period next) args))
      (:update-budget-last-7-days-by-payee
       (setf (alist-get 'budget-last-7-days-by-payee next) args))
      (:update-budget-last-30-days-by-payee
       (setf (alist-get 'budget-last-30-days-by-payee next) args))
      (:update-unbudgeted-last-7-days
       (setf (alist-get 'unbudgeted-last-7-days next) args))
      (:update-unbudgeted-last-30-days
       (setf (alist-get 'unbudgeted-last-30-days next) args))
      (:update-register-last-7-days
       (setf (alist-get 'register-last-7-days next) args))
      (_
       (error "Unknown action: %s" action)))

    next))


;; State accessors

(defmacro cb-ledger-reports--define-getter (attr)
  `(defun ,(intern (format "cb-ledger-reports-state-%s" attr)) (state)
     (alist-get (quote ,attr) state)))

(cb-ledger-reports--define-getter ledger-file)
(cb-ledger-reports--define-getter prev-pay-period)
(cb-ledger-reports--define-getter last-payday)


;; Async fetching

(defun cb-ledger-reports--fetch-value (state-property state args)
  (let ((ledger-file (cb-ledger-reports-state-ledger-file state))
        (buf (generate-new-buffer " async-value"))
        (err-buf (generate-new-buffer " cb-ledger-reports"))
        ;; `default-directory' must exist, otherwise `make-process' raises an
        ;; error.
        (default-directory (cb-ledger-reports--up-to-existing-dir default-directory)))

    (make-process
     :name "cb-ledger-reports"
     :buffer buf
     :stderr err-buf
     :command (append (list "ledger" "-f" ledger-file) args)
     :noquery t
     :sentinel
     (lambda (proc _status)
       (unwind-protect
           (let ((exit-code (process-exit-status proc)))
             (cond ((zerop exit-code)
                    (let ((action (intern (format ":update-%s" state-property))))
                      (cb-ledger-reports--state-update action (with-current-buffer buf (buffer-string)))))
                   (t
                    (let ((err-message (with-current-buffer err-buf (buffer-string))))
                      (unless (= 9 exit-code)
                        (message "Error: %s" err-message))))))
         (when proc
           (set-process-sentinel proc nil)
           (set-process-query-on-exit-flag proc nil)
           (let ((kill-buffer-query-functions nil)
                 (buf (process-buffer proc)))
             (ignore-errors (kill-process proc))
             (ignore-errors (delete-process proc))
             (ignore-errors (kill-buffer buf)))))))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (lambda ()
                                    (let ((kill-buffer-query-functions nil))
                                      (ignore-errors (kill-buffer err-buf))))
                nil t))))

(defun cb-ledger-reports--populate-state (state)
  (let ((last-payday (cb-ledger-reports-state-last-payday state))
        (prev-pay-period (cb-ledger-reports-state-prev-pay-period state)))
    (cb-ledger-reports--fetch-value 'assets-and-liabilities state '("bal" "^Assets" "^Liabilities" "--depth" "2"))
    (cb-ledger-reports--fetch-value 'expenses-last-7-days state '("bal" "expenses" "--sort" "total" "-p" "last 7 days" "--invert"))
    (cb-ledger-reports--fetch-value 'week-on-week-change state '("bal" "assets" "-p" "last 7 days"))
    (cb-ledger-reports--fetch-value 'budget-last-7-days state '("bal" "expenses" "--sort" "total" "-p" "last 7 days" "--invert" "--budget"))
    (cb-ledger-reports--fetch-value 'budget-last-30-days state '("bal" "expenses" "--sort" "total" "-p" "last 30 days" "--invert" "--budget"))
    (cb-ledger-reports--fetch-value 'budget-since-payday state `("bal" "expenses" "--sort" "total" "-b" ,last-payday "--invert""--budget"))
    (cb-ledger-reports--fetch-value 'budget-last-pay-period state `("bal" "expenses" "--sort" "total" "-p" ,prev-pay-period "--invert" "--budget"))
    (cb-ledger-reports--fetch-value 'budget-last-7-days-by-payee state '("reg" "expenses" "--by-payee" "--sort" "total" "-p" "last 7 days" "--invert" "--budget"))
    (cb-ledger-reports--fetch-value 'budget-last-30-days-by-payee state '("reg" "expenses" "--by-payee" "--sort" "total" "-p" "last 30 days" "--invert" "--budget"))
    (cb-ledger-reports--fetch-value 'unbudgeted-last-7-days state '("bal" "expenses" "--sort" "total" "-p" "last 7 days" "--invert" "--unbudgeted"))
    (cb-ledger-reports--fetch-value 'unbudgeted-last-30-days state '("bal" "expenses" "--sort" "total" "-p" "last 30 days" "--invert" "--unbudgeted"))
    (cb-ledger-reports--fetch-value 'register-last-7-days state '("reg" "checking" "-p" "last 7 days" "--invert"))))


;; Components

(defun cb-ledger-reports--up-to-existing-dir (dir)
  (while (not (file-directory-p dir))
    (setq dir (file-name-directory (directory-file-name dir))))
  dir)

(defun cb-ledger-reports--propertize-results (s)
  (with-temp-buffer
    (insert s)
    (font-lock-add-keywords
     nil
     `((,(rx "$" (* space) "-" (+ digit) (? "." (+ digit))) . 'cb-ledger-reports-negative-amount)
       (,(rx (+ digit) "-" (= 3 alpha) "-" (+ digit)) . 'ledger-font-posting-date-face)))
    (font-lock-ensure)
    (buffer-string)))

(blergh-define-component async-value (state-property state)
  (if-let (value (alist-get state-property state))
      (cb-ledger-reports--propertize-results value)
    '(propertize (face magit-dimmed) "Loading...")))

(blergh-define-component ledger-assets-and-liabilities (state)
  `(section (assets-and-liabilities nil)
            (heading "Assets & Liabilities")
            (indent
             (async-value assets-and-liabilities ,state))
            (padding)))

(blergh-define-component ledger-expenses-last-7-days (state)
  `(section (expenses-7-days nil)
            (heading "Expenses Last 7 Days")
            (indent
             (async-value expenses-last-7-days ,state))
            (padding)))

(blergh-define-component ledger-week-on-week-change (state)
  `(section (week-on-week nil)
            (heading "Week-on-week Change")
            (indent
             (line "How much money went in and out of my accounts?")
             (padding)
             (async-value week-on-week-change ,state))
            (padding)))

(blergh-define-component ledger-budget-last-7-days (state)
  `(section (budget-7-days nil)
            (heading "Last 7 days")
            (indent
             (async-value budget-last-7-days ,state))
            (padding)))

(blergh-define-component ledger-budget-last-30-days (state)
  `(section (budget-30-days nil)
            (heading "Last 30 days")
            (indent
             (async-value budget-last-30-days ,state))
            (padding)))

(blergh-define-component ledger-budget-since-payday (state)
  `(section (budget-since-payday nil)
            (heading "Since Payday")
            (indent
             (async-value budget-since-payday ,state))
            (padding)))

(blergh-define-component ledger-budget-last-pay-period (state)
  `(section (budget-last-pay-period nil)
            (heading "Previous Pay Period")
            (indent
             (async-value budget-last-pay-period ,state))
            (padding)))

(blergh-define-component ledger-budget-last-7-days-by-payee (state)
  `(section (budget-last-7-days nil)
            (heading "7 Days By Payee")
            (indent
             (async-value budget-last-7-days-by-payee ,state))
            (padding)))

(blergh-define-component ledger-budget-last-30-days-by-payee (state)
  `(section (budget-last-30-days nil)
            (heading "30 Days By Payee")
            (indent
             (async-value budget-last-30-days-by-payee ,state))
            (padding)))

(blergh-define-component ledger-unbudgeted-last-7-days (state)
  `(section (unbudgeted-last-7-days nil)
            (heading "7 Days")
            (indent
             (async-value unbudgeted-last-7-days ,state))
            (padding)))

(blergh-define-component ledger-unbudgeted-last-30-days (state)
  `(section (unbudgeted-last-30-days nil)
            (heading "30 Days")
            (indent
             (async-value unbudgeted-last-30-days ,state))
            (padding)))

(blergh-define-component ledger-register-last-7-days (state)
  `(section (register-7-days nil)
            (heading "Register (7 Days)")
            (indent
             (async-value register-last-7-days ,state))
            (padding)))

(blergh-define-component ledger-weekly-review (state)
  `((section (balances nil)
             (heading "Balances")
             (indent
              (line "Skim over balances to make sure they look right.")
              (padding)
              (ledger-assets-and-liabilities ,state)
              (ledger-expenses-last-7-days ,state)
              (ledger-week-on-week-change ,state))
             (padding))

    (section (budget nil)
             (heading "Budgeting")
             (indent
              (line "Skim the totals below, which are tallied against my budget.
- Am I meeting my budget?
- If not, what are the areas that need improvement?

These balances show the remaining available balance for each category.")
              (padding)
              (ledger-budget-last-7-days ,state)
              (ledger-budget-last-30-days ,state)
              (ledger-budget-since-payday ,state)
              (ledger-budget-last-pay-period ,state))
             (padding))

    (section (unbudgeted nil)
             (heading "Unbudgeted Spending")
             (indent
              (line "The payees below are organised by total spending against the budget.
- Any places where I tend to spend excessively?
- Any opportunity for savings?
- Any habits I could change to spend more wisely?")
              (padding)
              (ledger-unbudgeted-last-7-days ,state)
              (ledger-unbudgeted-last-30-days ,state))
             (padding))

    (section (register nil)
             (heading "Register")
             (indent
              (line "Read through the payees below from my checking account.
Any spending patterns here that could be budgeted?")
              (padding)
              (ledger-register-last-7-days ,state)))))


;; Commands

(defconst cb-ledger-reports-ledger-review-buffer "*ledger review*")

(defun cb-ledger-reports--redraw-review-buffer (state)
  (when-let (buf (get-buffer cb-ledger-reports-ledger-review-buffer))
    (with-current-buffer buf
      (blergh-mode)
      (let ((inhibit-read-only t)
            (pt (point)))
        (erase-buffer)
        (blergh-eval `(ledger-weekly-review ,state))
        (goto-char pt)))))

(defun cb-ledger-reports-weekly-review ()
  "Display the weekly review with STATE."
  (interactive)
  (let ((buf (get-buffer-create cb-ledger-reports-ledger-review-buffer))
        (state (cb-ledger-reports-state)))
    (add-hook 'cb-ledger-reports--redraw-functions #'cb-ledger-reports--redraw-review-buffer)
    (cb-ledger-reports--populate-state state)
    (cb-ledger-reports--redraw-review-buffer state)
    (pop-to-buffer buf)))


(provide 'cb-ledger-reports)

;;; cb-ledger-reports.el ends here
