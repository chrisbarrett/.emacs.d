;;; cb-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-emacs)
(require 'spacemacs-keys)

(use-package projectile
  :commands (projectile-ag
             projectile-compile-project
             projectile-invalidate-cache
             projectile-mode
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-project
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-test-project)

  :functions (projectile-project-p)

  :preface
  (progn
    (autoload 'magit-status "magit")
    (autoload '-const "dash-functional")

    (defun cb-projectile-test-project (arg)
      (interactive "P")
      (let ((compilation-buffer-name-function (-const "*projectile-test*")))
        (projectile-test-project arg))))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "p!" #'projectile-run-shell-command-in-root
      "p&" #'projectile-run-async-shell-command-in-root
      "pI" #'projectile-invalidate-cache
      "pa" #'projectile-ag
      "pD" #'projectile-dired
      "pc" #'projectile-compile-project
      "pr" #'projectile-replace
      "pt" #'cb-projectile-test-project
      "pu" #'projectile-run-project))

  :config
  (progn
    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action (lambda ()
                                             (dired (projectile-project-p))))
    (setq projectile-cache-file (concat cb-emacs-cache-directory "/projectile.cache"))
    (setq projectile-enable-caching t)

    (setq projectile-globally-ignored-files '("TAGS" ".DS_Store"))
    (setq projectile-globally-ignored-file-suffixes '("gz" "zip" "tar" "elc"))
    (setq projectile-globally-ignored-directories
          '(".bzr"
            ".ensime_cache"
            ".eunit"
            ".fslckout"
            ".g8"
            ".git"
            ".hg"
            ".idea"
            ".stack-work"
            ".svn"
            "build"
            "dist"
            "node_modules"
            "target"))

    (projectile-mode)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*projectile-test*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.2)))))

(use-package counsel-projectile
  :defer t
  :commands (counsel-projectile-on
             counsel-projectile-find-file
             counsel-projectile-find-dir
             counsel-projectile-switch-project
             counsel-projectile-switch-to-buffer
             counsel-projectile-rg)
  :init
  (spacemacs-keys-set-leader-keys
    "pf" #'counsel-projectile-find-file
    "pd" #'counsel-projectile-find-dir
    "pb" #'counsel-projectile-switch-to-buffer
    "pp" #'counsel-projectile-switch-project
    "/"  #'counsel-projectile-rg)

  :config
  (counsel-projectile-on))

(provide 'cb-projectile)

;;; cb-projectile.el ends here
