;;; config-ledger.el --- Configuration for ledger.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'general)
(require 'major-mode-hydra)
(require 's)

(autoload 'evil-insert-state "evil-states")
(autoload 'ledger-report "ledger")
(autoload 'org-read-date "org")

(major-mode-hydra-define ledger-mode nil
  ("Actions"
   (("r" #'ledger-report "report"))))


;; Utility functions

(defun config-ledger-insert-timestamp (date)
  "Insert DATE at point."
  (interactive (list (org-read-date)))
  (insert (s-replace "-" "/" date))
  (just-one-space)
  (evil-insert-state))

(defun config-ledger-report-from-report-buffer ()
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
  :general (:keymaps 'ledger-report-mode-map "C-c C-c" #'config-ledger-report-from-report-buffer)
  :general
  (:keymaps 'ledger-mode-map
   "C-c C-c" #'ledger-report
   "M-RET" #'ledger-toggle-current-transaction
   "C-c C-." #'config-ledger-insert-timestamp)
  :config
  (progn
    (general-setq
     ledger-accounts-file (expand-file-name "accounts.ledger" paths-ledger-directory)
     ledger-report-use-header-line nil
     ledger-post-account-alignment-column 2
     ledger-post-use-completion-engine :ido
     ledger-fontify-xact-state-overrides nil
     ledger-reports
     `(("budget" "ledger --depth 3 bal -p 'this month' ^Budget")
       ("assets & liabilities" "ledger bal --real '^Assets' '^Liabilities' --depth 2")
       ("balance" "ledger --depth 2 bal --real Assets Expenses Liablities Income")
       ("reg this week" "ledger reg --real checking -p 'this week'")
       ("reg this month" "ledger reg --real checking -p 'this month'")))

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

(use-package ledger-format
  :after ledger-mode
  :defer t
  :general (:keymaps 'ledger-mode-map "M-q" #'ledger-format-buffer))

(use-package flycheck-ledger
  :straight t
  :defer t
  :after ledger-mode)

(provide 'config-ledger)

;;; config-ledger.el ends here
