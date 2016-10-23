;;; cb-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(use-package projectile
  :commands (projectile-mode)
  :config
  (projectile-mode))

(use-package counsel-projectile
  :leader-bind
  (("p f" . counsel-projectile-find-file)
   ("p d" . counsel-projectile-find-dir)
   ("p b" . counsel-projectile-switch-to-buffer)
   ("p p" . counsel-projectile-switch-project)
   ("p a" . counsel-projectile-ag))

  :preface
  (autoload 'counsel-projectile-on "counsel-projectile")

  :init
  (progn
    (bind-key "s-l" #'counsel-projectile-ag)
    (bind-key "s-f" #'counsel-projectile-find-file))

  :config
  (counsel-projectile-on))

(provide 'cb-projectile)

;;; cb-projectile.el ends here
