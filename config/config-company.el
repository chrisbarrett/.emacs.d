;;; config-company.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

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
  (general-unbind :keymaps 'company-active-map "C-w")

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
   "C-p" #'company-box--prev-line
   "C-h" #'company-show-doc-buffer)
  :hook (company-mode . company-box-mode)
  :config
  (progn
    (setq company-box-enable-icon nil)
    (add-to-list 'company-box-frame-parameters '(width . 90)))

  ;; Update frame background colour on theme change.
  :config
  (progn
    (defun config-company--company-box-update-bg (&rest _)
      (with-selected-frame (company-box--get-frame)
        (set-background-color (face-background 'company-box-background nil t))))

    (advice-add 'company-box--display :after #'config-company--company-box-update-bg)))

(use-package company-dabbrev
  :after company
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(use-package company-quickhelp
  :straight t
  :after company
  :general (:keymaps 'company-active-map "C-h" #'company-quickhelp-manual-begin)
  :commands (company-quickhelp-mode)
  :config (company-quickhelp-mode +1))

(provide 'config-company)

;;; config-company.el ends here
