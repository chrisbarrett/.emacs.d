;;; cb-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(use-package projectile
  :commands (projectile-mode projectile-switch-project)
  :init
  (progn
    (bind-key "s-l" #'projectile-switch-project)

    (spacemacs-keys-set-leader-keys
      "pr" #'projectile-replace))
  :config
  (progn
    (setq projectile-switch-project-action #'magit-status)
    (projectile-mode)))

(use-package counsel-projectile
  :defer t
  :commands (counsel-projectile-on
             counsel-projectile-find-file
             counsel-projectile-find-dir
             counsel-projectile-switch-project
             counsel-projectile-switch-to-buffer
             counsel-projectile-ag)
  :init
  (progn
    (bind-key "s-f" #'counsel-projectile-find-file)

    (spacemacs-keys-set-leader-keys
      "pf" #'counsel-projectile-find-file
      "pd" #'counsel-projectile-find-dir
      "pb" #'counsel-projectile-switch-to-buffer
      "pp" #'counsel-projectile-switch-project
      "pa" #'counsel-projectile-ag
      "/"  #'counsel-projectile-ag))

  :config
  (counsel-projectile-on))

(provide 'cb-projectile)

;;; cb-projectile.el ends here
