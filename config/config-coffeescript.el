;;; config-coffeescript.el --- Configuration for CoffeeScript.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package coffee-mode
  :straight t
  :mode
  (("\\.coffee\\'" . coffee-mode)
   ("\\.iced\\'" . coffee-mode)
   ("Cakefile\\'" . coffee-mode)
   ("\\.cson\\'" . coffee-mode))

  :defines (coffee-tab-width)
  :interpreter ("coffee" . coffee-mode)
  :config
  (progn
    (setq coffee-tab-width 2)
    (setq coffee-indent-like-python-mode t)))

(provide 'config-coffeescript)

;;; config-coffeescript.el ends here
