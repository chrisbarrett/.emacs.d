;;; config-company.el --- Configuration for company-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package company
  :defer t
  :hook (after-init . global-company-mode)

  :general
  ([remap completion-at-point] #'company-manual-begin
   [remap complete-symbol] #'company-manual-begin)
  (:states '(insert normal emacs) :keymaps 'company-active-map
   "S-<return>" #'company-complete
   "<return>" #'company-complete-selection)
  (:keymaps 'comint-mode-map [remap indent-for-tab-command] #'company-manual-begin)

  :preface
  (general-unbind :keymaps 'company-active-map "C-w" "C-h")

  :preface
  (defun config-company--set-company-vars ()
    ;; HACK: evil-collection-company seems to be messing with this binding, so I
    ;; need to manually apply it again. :/
    (define-key company-active-map (kbd "RET") #'company-complete-selection)
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-align-annotations t))

  :custom
  ((company-idle-delay 0.3)
   (company-require-match nil))

  :config
  (progn
    (require 'company-tng)
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
