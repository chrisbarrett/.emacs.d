;;; config-ledger.el --- Configuration for ledger.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'general)
(require 's)
(require 'straight)

(autoload 'evil-insert-state "evil-states")
(autoload 'ledger-report "ledger")
(autoload 'org-read-date "org")

(general-setq ledger-master-file (file-truename "~/org/accounts.ledger"))


;; Utility functions

(defun cb-ledger-insert-timestamp (date)
  "Insert DATE at point."
  (interactive (list (org-read-date)))
  (insert (s-replace "-" "/" date))
  (just-one-space)
  (evil-insert-state))

(defun cb-ledger-report-from-report-buffer ()
  "Run a new report within the ledger report buffer."
  (interactive)
  (let ((buf (--first (with-current-buffer it
                        (derived-mode-p 'ledger-mode))
                      (buffer-list))))
    (pop-to-buffer buf)
    (call-interactively #'ledger-report)))


;; Package setup

(use-package ledger-mode
  :straight t
  :mode ("\\.ledger$" . ledger-mode)
  :general (:states 'normal :keymaps 'ledger-report-mode-map "q" #'kill-buffer-and-window)
  :general (:keymaps ledger-report-mode-map "C-c C-c" #'cb-ledger-report-from-report-buffer)
  :general
  (:keymaps 'ledger-mode-map
   "C-c C-c" #'ledger-report
   "M-RET" #'ledger-toggle-current-transaction
   "C-c C-." #'cb-ledger-insert-timestamp)
  :config
  (progn
    (general-setq
     ledger-report-use-header-line nil
     ledger-post-account-alignment-column 2
     ledger-post-use-completion-engine :ido
     ledger-fontify-xact-state-overrides nil
     ledger-reports
     `(("assets & liabilities" "ledger -f %(ledger-file) bal '^Assets' '^Liabilities' --depth 2")
       ("balance" "ledger -f %(ledger-file) --depth 3 bal not ^Equity")
       ("reg this week" "ledger -f %(ledger-file) reg checking -p 'this week'")
       ("reg this month" "ledger -f %(ledger-file) reg checking -p 'this month'")
       ("reg since payday" "ledger -f %(ledger-file) reg checking -b %(last-payday)")
       ("reg previous pay period" "ledger -f %(ledger-file) reg checking -p %(prev-pay-period)")))

    ;; Faces and font-locking

    (defface ledger-report-negative-amount
      `((t (:foreground "red")))
      "Face for negative amounts in ledger reports."
      :group 'ledger-faces)

    (font-lock-add-keywords
     'ledger-report-mode
     `((,(rx "$" (* space) "-" (+ digit) (? "." (+ digit))) . 'ledger-report-negative-amount)
       (,(rx (+ digit) "-" (= 3 alpha) "-" (+ digit)) . 'ledger-font-posting-date-face)))

    ;; Fix font lock issue in ledger reports
    (add-hook 'ledger-report-mode-hook 'font-lock-fontify-buffer)))

(use-package cb-ledger-format
  :after ledger-mode
  :general (:keymap 'ledger-mode-map "M-q" #'cb-ledger-format-buffer))

(use-package cb-ledger-reports
  :after ledger-mode
  :functions (cb-ledger-reports-last-n-pay-dates cb-ledger-reports-previous-pay-period))

(use-package flycheck-ledger
  :straight t
  :after ledger-mode)

(provide 'config-ledger)

;;; config-ledger.el ends here
