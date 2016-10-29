;;; cb-ledger.el --- Configuration for ledger.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(require 'spacemacs-keys)

(autoload 'evil-insert-state "evil-states")
(autoload 'org-read-date "org")
(autoload 'evil-define-key "evil-core")
(autoload 's-replace "s")

(defconst ledger-master-file (concat org-directory "/accounts.ledger"))

;; Utility functions

(defun cb-ledger-goto-ledger-file ()
  "Go to the ledger file."
  (interactive)
  (find-file ledger-master-file))

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
  :mode ("\.ledger$" . ledger-mode)

  :bind
  (:map
   ledger-mode-map
   ("C-c C-c" . ledger-report)
   ("M-RET" . ledger-toggle-current-transaction)
   ("C-c C-." . cb-ledger-insert-timestamp))

  :evil-bind
  (:state
   normal
   :map
   ledger-report-mode-map
   ("q" . kill-buffer-and-window))

  :init
  (spacemacs-keys-set-leader-keys "o$" #'cb-ledger-goto-ledger-file)

  :config
  (progn
    (setq ledger-post-account-alignment-column 2)
    (setq ledger-post-use-completion-engine :ido)
    (setq ledger-fontify-xact-state-overrides nil)

    (setq ledger-reports
          `(("assets" "ledger -f %(ledger-file) bal assets")
            ("balance" "ledger -f %(ledger-file) bal")
            ("reg this week" "ledger -f %(ledger-file) reg checking -p 'this week' --invert")
            ("reg this month" "ledger -f %(ledger-file) reg checking -p 'this month' --invert")
            ("reg since payday" "ledger -f %(ledger-file) reg checking -b %(last-payday) --invert")
            ("reg previous pay period" "ledger -f %(ledger-file) reg checking -p %(prev-pay-period) --invert")))

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
    (add-hook 'ledger-report-mode-hook 'font-lock-fontify-buffer)
    (define-key ledger-report-mode-map (kbd "C-c C-c") #'cb-ledger-report-from-report-buffer)))


(use-package cb-ledger-redefinitions
  :after ledger-report)

(use-package cb-ledger-format
  :after ledger-mode
  :bind (:map ledger-mode-map ("M-q" . cb-ledger-format-buffer)))

(use-package cb-ledger-reports
  :after ledger-mode
  :functions (cb-ledger-reports-init)
  :init
  (add-hook 'ledger-mode-hook #'cb-ledger-reports-init)
  :config
  (setq cb-ledger-reports-income-payee-name "Income:Movio"))

(use-package flycheck-ledger
  :after ledger-mode)

(provide 'cb-ledger)

;;; cb-ledger.el ends here
