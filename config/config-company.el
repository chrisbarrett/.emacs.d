;;; config-company.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'comint)
  (require 'use-package))

(use-package company
  :straight t
  :hook (after-init . global-company-mode)

  :bind
  (([remap completion-at-point] . company-manual-begin)
   ([remap complete-symbol] . company-manual-begin)
   ("S-<return>" . company-complete))

  :preface
  (defun config-company--set-company-vars ()
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-align-annotations t))

  :commands (company-select-next
             company-select-previous
             company-show-doc-buffer)

  :init
  (with-eval-after-load 'comint
    (define-key comint-mode-map [remap indent-for-tab-command]
      #'company-manual-begin))

  :config
  (progn
    (setq company-idle-delay 0.3)
    (setq company-require-match nil)

    (dolist (map (list company-active-map company-search-map company-filter-map))
      (define-key map (kbd "C-n") #'company-select-next)
      (define-key map (kbd "C-p") #'company-select-previous)
      (define-key map (kbd "C-h") #'company-show-doc-buffer)
      (define-key map (kbd "C-w") nil))

    (add-hook 'company-mode-hook #'config-company--set-company-vars)))

(use-package company-dabbrev
  :after company
  :config
  (progn
    (setq company-dabbrev-ignore-case nil)
    (setq company-dabbrev-downcase nil)))

(use-package company-quickhelp
  :straight t
  :after company
  :bind (:map company-active-map ("C-h" . company-quickhelp-manual-begin))
  :commands (company-quickhelp-mode)
  :config
  (company-quickhelp-mode +1))

(provide 'config-company)

;;; config-company.el ends here
