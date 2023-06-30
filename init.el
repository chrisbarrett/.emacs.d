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



(eval-when-compile
  (require 'use-package))

(defmacro use-config (feature &rest use-package-args)
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

(use-package server
  :if (not noninteractive)
  :demand t
  :config
  (server-start))

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-max-saved-items 100)
  (recentf-exclude (list (rx "TAGS" eos)
                         (rx ".DS_Store" eos)
                         (rx "." (or "gz" "zip" "tar" "7z") eos)
                         (rx bos "/sudo:root@")
                         (rx "/.git/")
                         (rx "/" (or "build" "dist" "target") "/")
                         (rx (or "/var/folders/"
                                 "/usr/local/Cellar/"
                                 "/tmp/"
                                 "/nix/store/"))
                         'file-remote-p
                         (regexp-quote no-littering-etc-directory)
                         (regexp-quote no-littering-var-directory))))

(use-package general
  :demand t
  :autoload (general-define-key))

(use-package delight
  :demand t)

(use-config cb-theme
  :autoload (cb-theme-apply-settings cb-theme-for-system-type)
  :defines (cb-light-theme cb-dark-theme cb-theme-mode-or-header-line-format)
  :config
  ;; Set reasonable placeholder foreground and background colours until the main
  ;; theme is loaded, according to the WM theme.
  (set-background-color (cb-theme-for-system-type :dark "#282c34" :light "#FDF6E3"))
  (set-foreground-color (cb-theme-for-system-type :dark "#bbc2cf" :light "#556b72"))

  (setq-default header-line-format cb-theme-mode-or-header-line-format)
  (setq-default mode-line-format nil)

  (cb-theme-apply-settings)
  (load-theme (cb-theme-for-system-type :light cb-theme-light :dark cb-theme-dark) t)

  ;; KLUDGE: Something weird is clobbering settings in org-mode. Reapply the user
  ;; theme when starting up org-mode.
  (add-hook 'org-mode-hook #'cb-theme-apply-settings))

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

(use-config cb-display-buffer)

;;; init.el ends here
