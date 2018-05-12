;;; config-navigation.el --- Configuration for avy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(spacemacs-keys-declare-prefix "j" "jumping")
(spacemacs-keys-declare-prefix "x" "urls")

(spacemacs-keys-set-leader-keys "g SPC" #'pop-tag-mark)



(use-package avy
  :straight t
  :bind (:map
         spacemacs-keys-default-map
         ("jb" . avy-pop-mark)
         ("jj" . evil-avy-goto-char)
         ("jJ" . evil-avy-goto-char-2)
         ("jl" . evil-avy-goto-line)
         ("jw" . evil-avy-goto-word-or-subword-1))
  :config
  (progn
    (setq avy-all-windows 'all-frames)
    (setq avy-background t)))

(use-package link-hint
  :straight t
  :bind
  (:map
   spacemacs-keys-default-map
   ("xo" . link-hint-open-link)
   ("xO" . link-hint-open-multiple-links)))

(use-package dumb-jump
  :straight t
  :commands (dumb-jump-go dumb-jump-go-other-window)
  :bind (:map
         spacemacs-keys-default-map
         ("gg" . dumb-jump-go)
         ("go"  . dumb-jump-go-other-window))

  :preface
  (autoload 'pop-tag-mark "etags")
  :init
  (evil-define-key 'normal prog-mode-map (kbd "M-.") #'dumb-jump-go)
  :config
  (setq dumb-jump-selector 'ivy))

(use-package tiny
  :straight t
  :bind ("C-:" . tiny-expand))

(provide 'config-navigation)

;;; config-navigation.el ends here
