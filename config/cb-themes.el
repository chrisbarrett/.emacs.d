;;; cb-themes.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

;; Load themes.

(defvar cb-themes-dark-mode-p t)

(defun cb-themes/toggle-dark-mode ()
  "Toggle between light and dark mode."
  (interactive)
  (if cb-themes-dark-mode-p
      (cb-light-theme)
    (cb-dark-theme))
  (setq cb-themes-dark-mode-p (not cb-themes-dark-mode-p)))

(defun cb-light-theme ()
  (interactive)
  (load-theme 'cb-light t))

(defun cb-dark-theme ()
  (interactive)
  (load-theme 'cb-dark t))

(cb-dark-theme)

(spacemacs-keys-set-leader-keys
  "t t" #'cb-themes/toggle-dark-mode
  "t d" #'cb-dark-theme
  "t l" #'cb-light-theme)


;; Configure packages

(use-package cb-ligatures
  :if (display-graphic-p)
  :functions (cb-ligatures-init)
  :config
  (progn
    (add-hook 'prog-mode-hook #'cb-ligatures-init)
    (add-hook 'text-mode-hook #'cb-ligatures-init)
    (add-hook 'org-agenda-mode-hook #'cb-ligatures-init)
    (global-prettify-symbols-mode +1)))

(provide 'cb-themes)

;;; cb-themes.el ends here
