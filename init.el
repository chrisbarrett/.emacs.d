;;; init.el --- User init file for Emacs  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar org-roam-index-node-id)



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
  (cl-assert (file-exists-p (expand-file-name (format "./config/%s.el" feature)
                                              user-emacs-directory)))
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
  :autoload (cb-autoloads-build-and-load)
  :config (cb-autoloads-build-and-load))

(use-package general
  :demand t
  :autoload (general-unbind general-def general-define-key)
  :config
  (general-def
    "M-t" 'transpose-words
    "M-SPC" 'cycle-spacing
    "C-c e e" 'toggle-debug-on-error
    "C-c e q" 'toggle-debug-on-quit)

  (general-def :keymaps '(minibuffer-local-map
                          minibuffer-local-ns-map
                          minibuffer-local-completion-map
                          minibuffer-local-must-match-map
                          minibuffer-local-isearch-map)
    "<escape>" 'keyboard-escape-quit)

  (general-unbind
    "<f2>" ; 2-window scrolling
    "S-<f2>" ; 2-window scrolling
    "C-z" ; suspend-frame
    "s-t" ; macOS font panel
    ))

(use-package server
  :if (not noninteractive)
  :demand t
  :config
  (server-start))

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

(when (file-exists-p custom-file)
  (load custom-file t t))

;;; init.el ends here
