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
   (("r" #'ledger-report "report..."))))

(major-mode-hydra-define ledger-report-mode nil
  ("Actions"
   (("e" #'ledger-report-edit-report "edit...")
    ("r" #'ledger-report "report..."))))


;; Package setup

(use-package ledger-mode
  :straight t
  :mode ("\\.ledger$" . ledger-mode)
  :general

  (:keymaps 'ledger-report-mode-map
   "C-c C-c" #'ledger-report
   "q"       #'kill-buffer-and-window)

  (:keymaps 'ledger-mode-map
   "C-c C-c" #'ledger-report
   "M-RET" #'ledger-toggle-current-transaction
   "C-c C-." #'config-ledger-set-xact-timestamp)

  :preface
  (defun config-ledger-set-xact-timestamp ()
    (interactive)
    (when-let* ((ctx (ledger-xact-context))
                (value (ledger-context-field-value ctx 'date))
                (start (ledger-context-field-position ctx 'date))
                (end (ledger-context-field-end-position ctx 'date))
                (updated (ledger-read-date "Transaction date: ")))
      (if (string= value updated)
          (user-error "Date unchanged")
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert updated))
        (message "Date changed: %s -> %s" value updated))))

  :config
  (progn
    (general-setq
     ledger-report-use-header-line nil
     ledger-post-account-alignment-column 2
     ledger-fontify-xact-state-overrides nil)

    ;; Faces and font-locking

    (defface ledger-report-negative-amount
      `((t (:foreground "red")))
      "Face for negative amounts in ledger reports."
      :group 'ledger-faces)

    (font-lock-add-keywords
     'ledger-report-mode
     `((,(rx "$" (* space) "-" (+ digit) (* (any digit ",")) (? "." (+ digit))) . 'ledger-report-negative-amount)
       (,(rx (+ digit) "-" (= 3 alpha) "-" (+ digit)) . 'ledger-font-posting-date-face)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Ledger Report*" eos)
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (side . right)
                   (window-width . 80)))

    ;; Fix font lock issue in ledger reports
    (add-hook 'ledger-report-mode-hook 'font-lock-fontify-buffer)))

(use-package ledger-format
  :after ledger-mode
  :defer t
  :general (:keymaps 'ledger-mode-map "M-q" #'ledger-format-buffer))

(use-package flycheck-ledger
  :straight t
  :after (:and flycheck ledger-mode))

(provide 'config-ledger)

;;; config-ledger.el ends here
