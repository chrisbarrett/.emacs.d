;;; config-themes.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)



(defvar config-themes--dark-mode-p t)

(defun config-themes/toggle-dark-mode ()
  "Toggle between light and dark mode."
  (interactive)
  (if config-themes--dark-mode-p
      (config-themes/light-theme)
    (config-themes/dark-theme))
  (setq config-themes--dark-mode-p (not config-themes--dark-mode-p)))

(defun config-themes/light-theme ()
  (interactive)
  (load-theme 'cb-light t))

(defun config-themes/dark-theme ()
  (interactive)
  (load-theme 'cb-dark t))

(spacemacs-keys-set-leader-keys
  "t t" #'config-themes/toggle-dark-mode
  "t d" #'config-themes/dark-theme
  "t l" #'config-themes/light-theme)



(use-package page-break-lines
  :straight t
  :commands (global-page-break-lines-mode)
  :demand t
  :config
  (progn
    (setq page-break-lines-modes
          '(prog-mode
            text-mode
            compilation-mode
            help-mode
            org-agenda-mode))

    (global-page-break-lines-mode)))

(use-package ligatures
  :if (display-graphic-p)
  :hook
  ((prog-mode . ligatures-init)
   (text-mode . ligatures-init)
   (org-agenda-mode . ligatures-init))
  :config
  (global-prettify-symbols-mode +1))

(provide 'config-themes)

;;; config-themes.el ends here
