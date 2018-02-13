;;; cb-faces.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

;; Load themes.

(let ((this-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'custom-theme-load-path (concat this-dir "cb-faces/")))

(defvar cb-faces-dark-mode-p t)

(defun cb-faces/toggle-dark-mode ()
  "Toggle between light and dark mode."
  (interactive)
  (if cb-faces-dark-mode-p
      (cb-light-theme)
    (cb-dark-theme))
  (setq cb-faces-dark-mode-p (not cb-faces-dark-mode-p)))

(defun cb-light-theme ()
  (interactive)
  (load-theme 'cb-light t))

(defun cb-dark-theme ()
  (interactive)
  (load-theme 'cb-dark t))

(cb-dark-theme)

(spacemacs-keys-set-leader-keys
  "t t" #'cb-faces/toggle-dark-mode
  "t g" #'cb-green-theme
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
    (global-prettify-symbols-mode +1)))

(use-package page-break-lines
  :commands (global-page-break-lines-mode)
  :demand t
  :config
  (global-page-break-lines-mode))

(provide 'cb-faces)

;;; cb-faces.el ends here
