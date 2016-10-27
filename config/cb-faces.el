;;; cb-faces.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'spacemacs-keys)

(defvar cb-faces-dark-mode-p nil)

(defun cb-faces/toggle-dark-mode ()
  "Toggle between light and dark mode."
  (interactive)
  (if cb-faces-dark-mode-p
      (load-theme 'cb-light t)
    (load-theme 'cb-dark t))
  (setq cb-faces-dark-mode-p (not cb-faces-dark-mode-p)))

(spacemacs-keys-set-leader-keys "t t" #'cb-faces/toggle-dark-mode)

(let ((this-dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'custom-theme-load-path (concat this-dir "cb-faces/")))

(load-theme 'cb-light t)

(provide 'cb-faces)

;;; cb-faces.el ends here
