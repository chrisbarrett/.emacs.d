;;; cb-dumb-jump.el --- Configuration for dumb-jump  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'evil)
(require 'spacemacs-keys)

(autoload 'pop-tag-mark "etags")

(use-package dumb-jump
  :straight t
  :commands (dumb-jump-go dumb-jump-go-other-window)
  :init
  (progn
    (evil-define-key 'normal prog-mode-map (kbd "M-.") #'dumb-jump-go)

    (spacemacs-keys-set-leader-keys
      "g SPC" #'pop-tag-mark
      "g g" #'dumb-jump-go
      "g o"  #'dumb-jump-go-other-window))

  :config
  (setq dumb-jump-selector 'ivy))

(provide 'cb-dumb-jump)

;;; cb-dumb-jump.el ends here
