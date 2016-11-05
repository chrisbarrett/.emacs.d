;;; cb-dired.el --- Configuration for dired.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(autoload 'evil-define-key "evil-core")

(use-package dired
  :defer t
  :commands (dired dired-hide-details-mode)
  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'dired-mode "s" "subdir")
    (spacemacs-keys-set-leader-keys "d" #'dired)
    (spacemacs-keys-set-leader-keys-for-major-mode
      'dired-mode
      "si" 'dired-insert-subdir
      "sd" 'dired-kill-subdir)
    (add-hook 'dired-mode-hook #'dired-hide-details-mode)))

(use-package dired-x
  :commands (dired-omit-mode)
  :init
  (progn
    (add-hook 'dired-load-hook (lambda () (load "dired-x")))
    (spacemacs-keys-set-leader-keys-for-major-mode
      'dired-mode
      "h" #'dired-omit-mode)

    (add-hook 'dired-mode-hook #'dired-omit-mode))
  :config
  (progn
    (evil-define-key 'normal dired-mode-map (kbd "h") #'dired-omit-mode)
    (setq dired-omit-files (rx bol (+ ".")))))


(provide 'cb-dired)

;;; cb-dired.el ends here
