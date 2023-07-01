;;; init.el --- User init file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Configure load-path

(dolist (load-dir (list
                   "/run/current-system/sw/share/emacs/site-lisp"
                   "~/.nix-profile/share/emacs/site-lisp"
                   (expand-file-name "lisp/" user-emacs-directory)
                   (expand-file-name "lisp/nursery/lisp/" user-emacs-directory)
                   ))
  (when (file-directory-p load-dir)
    (add-to-list 'load-path load-dir)))



;;; Define a helper for loading features in ./config.

(require 'cl-lib)

(defmacro use-config (feature &rest use-package-args)
  "Load FEATURE from ./config with some default `use-package' args.

USE-PACKAGE-ARGS are optional additional arguments forwarded to
`use-package'."
  (declare (indent 1))
  (let ((file (expand-file-name (format "./config/%s.el" feature)
                                user-emacs-directory)))
    (cl-assert (file-exists-p file) t))
  `(use-package ,feature
     :load-path "./config/" :demand t ,@use-package-args))

(font-lock-add-keywords 'emacs-lisp-mode
                        `((,(rx "("
                                (group "use-config") symbol-end (* (any space))
                                (group (+ (or (syntax word) (syntax symbol))))
                                (? ")"))
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))))



;;; Load features

(use-package no-littering
  :demand t
  :autoload (no-littering-theme-backups)
  :config (no-littering-theme-backups))

(use-config cb-startup-profiling-and-debugging)
(use-config cb-gc-tuning)

(use-config cb-autoloads
  :config
  (with-no-warnings
    (cb-autoloads-build-and-load))
  (use-package autoloads :demand t))

(use-package server
  :if (not noninteractive)
  :demand t
  :config
  (server-start))

(use-package delight :demand t)
(use-package general :demand t)

(use-config cb-theme)
(use-config cb-completion)
(use-config cb-window-management)
(use-config cb-input)
(use-config cb-general-editing)
(use-config cb-help-systems)
(use-config cb-search)
(use-config cb-snippets)
(use-config cb-media)
(use-config cb-evil)
(use-config cb-leader)
(use-config cb-ide)
(use-config cb-eshell)
(use-config cb-smartparens)
(use-config cb-dired)
(use-config cb-git)
(use-config cb-langs)
(use-config cb-lang-elisp)
(use-config cb-lang-typescript)
(use-config cb-lang-rust)

(load custom-file t t)

;;; init.el ends here
