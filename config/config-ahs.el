;;; config-ahs.el --- Configure auto-highlight-symbol.  -*- lexical-binding: t; -*-

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
  :hook (prog-mode . auto-highlight-symbol-mode)
  :config
  (progn
    (setq ahs-case-fold-search nil)
    (setq ahs-default-range 'ahs-range-whole-buffer)
    (setq ahs-idle-interval 0.25)
    (setq ahs-inhibit-face-list nil)

    ;; Disable by default, use keybinding instead.
    (setq ahs-idle-timer 0)))

(use-package evil-ahs
  :commands (evil-ahs/highlight-symbol evil-ahs/goto-last-searched-symbol)
  :bind (:map
         evil-motion-state-map
         ("*" . evil-ahs/enter-ahs-forward)
         ("#" . evil-ahs/enter-ahs-backward)
         :map
         spacemacs-keys-default-map
         ("sh" . evil-ahs/highlight-symbol)
         ("sH" . evil-ahs/goto-last-searched-symbol)))

(provide 'config-ahs)

;;; config-ahs.el ends here
