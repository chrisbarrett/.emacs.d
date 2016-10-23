;;; cb-evil.el --- Configuration for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package evil
  :preface
  (defun cb-evil--sp-delete-and-join-compat (fn &rest args)
    (if (bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char)
      (apply fn args)))

  :config
  (progn
    (evil-mode +1)
    (setq-default evil-shift-width 2)

    (setq evil-want-Y-yank-to-eol t)

    ;; Configure cursors.

    (setq evil-motion-state-cursor '("plum3" box))
    (setq evil-visual-state-cursor '("gray" (hbar . 2)))
    (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
    (setq evil-emacs-state-cursor  '("SkyBlue2" hbar))

    ;; Motion keys for help buffers.

    (evil-define-key 'motion help-mode-map (kbd "<escape>") #'quit-window)
    (evil-define-key 'motion help-mode-map (kbd "<tab>") #'forward-button)
    (evil-define-key 'motion help-mode-map (kbd "S-<tab>") #'backward-button)
    (evil-define-key 'motion help-mode-map (kbd "]") #'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "gf") #'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "[") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gb") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gh") #'help-follow-symbol)


    ;; Better compat with smartparens-strict mode.
    ;; TODO: Move to SP config.

    (advice-add #'evil-delete-backward-char-and-join
                :around #'cb-evil--sp-delete-and-join-compat))

  :functions (evil-mode evil-delay evil-delete-backward-char-and-join)
  :defines (evil-want-Y-yank-to-eol))

(use-package evil-terminal-cursor-changer
  :after evil
  :if (not (display-graphic-p))
  :config (evil-terminal-cursor-changer-activate)
  :functions (evil-terminal-cursor-changer-activate))

(use-package evil-surround
  :after evil
  :config
  (setq-default evil-surround-pairs-alist
                '((?\( . ("(" . ")"))
                  (?\[ . ("[" . "]"))
                  (?\{ . ("{" . "}"))

                  (?\) . ("(" . ")"))
                  (?\] . ("[" . "]"))
                  (?\} . ("{" . "}"))

                  (?# . ("#{" . "}"))
                  (?b . ("(" . ")"))
                  (?B . ("{" . "}"))
                  (?> . ("<" . ">"))
                  (?t . surround-read-tag)
                  (?< . surround-read-tag)
                  (?f . surround-function))))


(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (spacemacs-keys-set-leader-keys "se" 'evil-iedit-state/iedit-mode)

  :config
  (progn
    (setq iedit-current-symbol-default t)
    (setq iedit-only-at-symbol-boundaries t)
    (setq iedit-toggle-key-default nil)

    ;; Enable leader key in iedit and iedit-insert states
    (define-key evil-iedit-state-map (kbd "SPC") spacemacs-keys-default-map)))


(provide 'cb-evil)

;;; cb-evil.el ends here
