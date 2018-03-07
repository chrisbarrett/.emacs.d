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
    (autoload 'projectile-register-project-type "projectile")

    (defconst cb-projectile-ignored-base-dirs
      '("/nix/store/"))

    (defun cb-projectile--file-is-child-of-test-dir (&optional has-test-prefix-or-suffix)
      (or has-test-prefix-or-suffix
          (when-let* ((file (buffer-file-name)))
            (seq-contains (f-split file) "test"))))

    (defun cb-projectile--substitute-test-with-impl-dir (&optional existing)
      (or existing
          (when-let* ((file (buffer-file-name))
                      (impl-dir (if (f-dir? (f-join (projectile-project-root) "lib"))
                                    "/lib/"
                                  "/src/")))

            (s-replace "/test/" impl-dir file))))

    (defun cb-projectile--substitute-impl-with-test-dir (&optional existing)
      (or existing
          (when-let* ((file (buffer-file-name)))
            (replace-regexp-in-string (rx "/" (or "lib" "src") "/") "/test/" file t t))))

    (defun cb-projectile--project-is-ignored-subdir-p (project)
      (seq-find (lambda (base)
                  (f-child-of-p project base))
                cb-projectile-ignored-base-dirs))

    (defun cb-projectile-test-project (arg)
      (interactive "P")
      (let ((compilation-buffer-name-function (-const "*projectile-test*")))
        (projectile-test-project arg))))

  :init
  (progn
    (defvar projectile-cache-file (concat cb-emacs-cache-directory "/projectile.cache"))
    (defvar projectile-known-projects-file (concat cb-emacs-cache-directory "/projectile-bookmarks.eld"))

    (spacemacs-keys-set-leader-keys
      "p TAB" #'projectile-toggle-between-implementation-and-test
      "p <backtab>" #'projectile-find-implementation-or-test-other-window
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
    (setq projectile-cache-file (concat cb-emacs-cache-directory "/projectile.cache"))
    (setq projectile-known-projects-file (concat cb-emacs-cache-directory "/projectile-bookmarks.eld"))
    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action (lambda ()
                                             (dired (projectile-project-p))))
    (setq projectile-enable-caching t)
    (setq projectile-create-missing-test-files t)

    (setq projectile-globally-ignored-files '("TAGS" ".DS_Store"))
    (setq projectile-globally-ignored-file-suffixes '("gz" "zip" "tar" "elc"))
    (setq projectile-ignored-project-function #'cb-projectile--project-is-ignored-subdir-p)

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
            "jars"
            "target"))

    (projectile-mode)

    (projectile-register-project-type 'yarn '("yarn.lock")
                                      :compile "yarn build"
                                      :test "yarn test")

    ;; Teach projectile how to resolve npm srcs and tests.
    (advice-add #'projectile-test-file-p :filter-return #'cb-projectile--file-is-child-of-test-dir)
    (advice-add #'projectile-find-matching-file :filter-return #'cb-projectile--substitute-test-with-impl-dir)
    (advice-add #'projectile-find-matching-test :filter-return #'cb-projectile--substitute-impl-with-test-dir)

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
  :commands (counsel-projectile-mode
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
    "/"  #'counsel-projectile-rg)

  :config
  (counsel-projectile-mode))

(provide 'cb-projectile)

;;; cb-projectile.el ends here
