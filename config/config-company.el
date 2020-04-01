;;; config-company.el --- Configuration for company-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :straight t
  :defer t
  :hook (after-init . global-company-mode)

  :general
  ([remap completion-at-point] #'company-manual-begin
   [remap complete-symbol] #'company-manual-begin
   "S-<return>" #'company-complete
   :keymaps 'comint-mode-map
   [remap indent-for-tab-command] #'company-manual-begin)

  :preface
  (general-unbind :keymaps 'company-active-map "C-w" "C-h")

  :preface
  (defun config-company--set-company-vars ()
    ;; HACK: evil-collection-company seems to be messing with this binding, so I
    ;; need to manually apply it again. :/
    (define-key company-active-map (kbd "RET") #'company-complete-selection)
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-align-annotations t))

  :config
  (progn
    (with-eval-after-load 'evil
      (evil-define-key 'insert company-mode-map (kbd "S-<return>") #'company-complete))
    (setq company-idle-delay 0.3)
    (setq company-require-match nil)
    (add-hook 'company-mode-hook #'config-company--set-company-vars)))

(use-package company-dabbrev
  :after company
  :defer t
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(provide 'config-company)

;;; config-company.el ends here
