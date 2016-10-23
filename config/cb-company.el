;;; cb-company.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(use-package company
  :commands (global-company-mode)

  :bind
  (("S-<return>" . company-complete))

  :preface
  (defun cb-company--set-company-vars ()
    (setq company-minimum-prefix-length 3)
    (setq company-tooltip-align-annotations t))

  :init
  (add-hook 'after-init-hook #'global-company-mode)

  :config
  (progn
    (dolist (map (list company-active-map company-search-map company-filter-map))
      (define-key map (kbd "C-n") #'company-select-next)
      (define-key map (kbd "C-p") #'company-select-previous)
      (define-key map (kbd "C-h") #'company-show-doc-buffer)
      (define-key map (kbd "C-w") nil))

    (add-hook 'company-mode-hook #'cb-company--set-company-vars)))

(provide 'cb-company)

;;; cb-company.el ends here
