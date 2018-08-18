;;; config-company.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'company-box-hacks)

(use-package company
  :straight t
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
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-align-annotations t))

  :config
  (progn
    (setq company-idle-delay 0.3)
    (setq company-require-match nil)
    (add-hook 'company-mode-hook #'config-company--set-company-vars)))

(use-package company-box
  :straight t
  :general
  (:keymaps '(company-box-mode-map company-active-map company-search-map company-filter-map)
   "C-n" #'company-box--next-line
   "C-p" #'company-box--prev-line)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-enable-icon nil))

(use-package company-dabbrev
  :after company
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(provide 'config-company)

;;; config-company.el ends here
