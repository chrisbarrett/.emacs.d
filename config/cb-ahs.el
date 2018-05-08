;;; cb-ahs.el --- Configure auto-highlight-symbol.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

;; Prevent the default keymap from getting created
(defvar auto-highlight-symbol-mode-map (make-sparse-keymap))

(use-package auto-highlight-symbol
  :straight t
  :defer t
  :commands (auto-highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode)
  :config
  (progn
    (setq ahs-case-fold-search nil)
    (setq ahs-default-range 'ahs-range-whole-buffer)
    (setq ahs-idle-interval 0.25)
    (setq ahs-inhibit-face-list nil)

    ;; Disable by default, use keybinding instead.
    (setq ahs-idle-timer 0)))

(use-package cb-ahs-micro-state
  :commands (cb-ahs-micro-state/highlight-symbol
             cb-ahs-micro-state/enter-ahs-forward
             cb-ahs-micro-state/enter-ahs-backward
             cb-ahs-micro-state/goto-last-searched-symbol)
  :defer t
  :init
  (progn
    (with-eval-after-load 'evil
      (define-key evil-motion-state-map (kbd "*") #'cb-ahs-micro-state/enter-ahs-forward)
      (define-key evil-motion-state-map (kbd "#") #'cb-ahs-micro-state/enter-ahs-backward))

    (spacemacs-keys-set-leader-keys
      "sh" #'cb-ahs-micro-state/highlight-symbol
      "sH" #'cb-ahs-micro-state/goto-last-searched-symbol)))

(provide 'cb-ahs)

;;; cb-ahs.el ends here
