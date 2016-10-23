;;; cb-ws-butler.el --- Configure ws-butler.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(use-package ws-butler
  :commands (ws-butler-global-mode) 
  :defer 1
  :config
  (ws-butler-global-mode)) 

(provide 'cb-ws-butler)

;;; cb-ws-butler.el ends here
