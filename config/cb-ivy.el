;;; cb-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(use-package ivy
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer))

  :leader-bind
  (("r" . ivy-resume)
   ("b b" . ivy-switch-buffer))

  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (ivy-mode))

  :demand t
  :functions (ivy-mode)
  :defines (ivy-use-virtual-buffers ivy-count-format))


(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-h v" . counsel-describe-variable)
   ("C-h f" . counsel-describe-function))

  :leader-bind
  (("<SPC>" . counsel-M-x)
   ("f f" . counsel-find-file)
   ("h d f" . counsel-describe-function)
   ("h d v" . counsel-describe-variable))

  :config
  (define-key read-expression-map (kbd "C-r") #'counsel-expression-history)

  :demand t
  :functions (counsel-expression-history))

(provide 'cb-ivy)

;;; cb-ivy.el ends here
