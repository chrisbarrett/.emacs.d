;;; config-ws-butler.el --- Configure ws-butler.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ws-butler
  :straight t
  :commands (ws-butler-global-mode)
  :defer 3
  :config
  (ws-butler-global-mode))

(provide 'config-ws-butler)

;;; config-ws-butler.el ends here
