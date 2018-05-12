;;; config-navigation.el --- Configuration for avy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(spacemacs-keys-declare-prefix "x" "urls")



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
  :preface
  (autoload 'pop-tag-mark "etags")
  :init
  (evil-define-key 'normal prog-mode-map (kbd "M-.") #'dumb-jump-go)
  :config
  (setq dumb-jump-selector 'ivy))

(provide 'config-navigation)

;;; config-navigation.el ends here
