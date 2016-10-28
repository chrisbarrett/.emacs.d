;;; cb-ibuffer.el --- Configuration for ibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'evilified-state)

(use-package ibuffer
  :commands (ibuffer ibuffer-forward-line ibuffer-backward-line)
  :bind ("C-x C-b" . ibuffer-other-window)
  :init
  (spacemacs-keys-set-leader-keys "b l" #'ibuffer)
  :config
  (progn
    (setq ibuffer-expert t)
    (define-key ibuffer-mode-map (kbd "SPC") spacemacs-keys-default-map)
    (define-key ibuffer-mode-map (kbd "j") #'ibuffer-forward-line)
    (define-key ibuffer-mode-map (kbd "k") #'ibuffer-backward-line)))

(use-package ibuf-ext
  :commands (ibuffer-auto-mode)
  :init
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-projectile
  :commands (ibuffer-projectile-set-filter-groups)
  :functions (ibuffer-do-sort-by-alphabetic)
  :preface
  (defun cb-ibuffer--setup-buffer ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :init
  (add-hook 'ibuffer-hook #'cb-ibuffer--setup-buffer))

(provide 'cb-ibuffer)

;;; cb-ibuffer.el ends here
