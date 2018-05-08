;;; cb-avy.el --- Configuration for avy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'evil-avy-goto-char "evil-integration")
(autoload 'evil-avy-goto-char-2 "evil-integration")
(autoload 'evil-avy-goto-line "evil-integration")
(autoload 'evil-avy-goto-word-or-subword-1 "evil-integration")

(use-package avy
  :straight t
  :defer t
  :commands (avy-pop-mark)

  :preface
  (progn
    (autoload 'evil-avy-goto-char "evil-integration")
    (autoload 'evil-avy-goto-char-2 "evil-integration")
    (autoload 'evil-avy-goto-line "evil-integration")
    (autoload 'evil-avy-goto-word-or-subword-1 "evil-integration"))

  :init
  (progn
    (spacemacs-keys-declare-prefix "j" "jumping")
    (spacemacs-keys-set-leader-keys
      "jb" #'avy-pop-mark
      "jj" #'evil-avy-goto-char
      "jJ" #'evil-avy-goto-char-2
      "jl" #'evil-avy-goto-line
      "jw" #'evil-avy-goto-word-or-subword-1))

  :config
  (progn
    (setq avy-all-windows 'all-frames)
    (setq avy-background t)))

(use-package link-hint
  :straight t
  :defer t
  :commands (link-hint-open-link
             link-hint-open-multiple-links)
  :init
  (progn
    (spacemacs-keys-declare-prefix "x" "urls")
    (spacemacs-keys-set-leader-keys
      "xo" #'link-hint-open-link
      "xO" #'link-hint-open-multiple-links)))

(provide 'cb-avy)

;;; cb-avy.el ends here
