;;; init.el --- User init file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:

;; Any package with :ensure will be automatically installed via Nix using the
;; Emacs overlay.

;;; Code:

;; Declare some utility packages this config depends on.

(use-package dash :ensure t)
(use-package delight :ensure t :demand t)
(use-package f :ensure t)
(use-package general :ensure t :demand t)
(use-package ht :ensure t)
(use-package memoize :ensure t)
(use-package s :ensure t)
(use-package ts :ensure t)

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

(use-package server
  :if (not noninteractive)
  :demand t
  :config
  (server-start))

;;; Load configuration

(use-config cb-parameters)
(use-config cb-startup-profiling-and-debugging)
(use-config cb-gc-tuning)

(use-config cb-ensured-packages
  :hook (after-init . cb-ensured-packages-write))

(use-config cb-autoloads
  :config
  (with-no-warnings
    (cb-autoloads-build-and-load))
  (use-package autoloads :demand t))

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
(use-config cb-lang-clojure)
(use-config cb-lang-elisp)
(use-config cb-lang-latex)
(use-config cb-lang-markdown)
(use-config cb-lang-rust)
(use-config cb-lang-typescript)
(use-config cb-org)

(use-config cb-org-roam
  :autoload cb-org-roam-initial-buffers
  :custom
  (initial-buffer-choice #'cb-org-roam-initial-buffers))

(load (expand-file-name "lisp/init.el" org-directory) t t)

(load custom-file t t)

;;; init.el ends here
