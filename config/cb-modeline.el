;;; cb-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package hidden-mode-line
  :commands (hidden-mode-line-mode global-hidden-mode-line-mode)
  :init
  (setq-default mode-line-format " "))

(use-package cb-header-line-mode
  :commands (cb-header-line-mode cb-header-line-mode-on)
  :init
  (progn
    (spacemacs-keys-set-leader-keys "tm" #'cb-header-line-mode)
    (add-hook 'term-mode-hook #'cb-header-line-mode-on)))

(provide 'cb-modeline)

;;; cb-modeline.el ends here
