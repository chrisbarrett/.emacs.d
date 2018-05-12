;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "26")
  (error "This version of Emacs is not supported"))

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setenv "INSIDE_EMACS" "true")


;; Make sure package.el doesn't get a chance to load anything.

(setq package-enable-at-startup nil)


;; Bootstrap straight.el package manager.

(eval-and-compile
  (defvar bootstrap-version 3)
  (defvar bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el")))

(unless (file-exists-p bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq straight-cache-autoloads t)
(setq straight-check-for-modifications 'live)

(require 'straight bootstrap-file t)


;; Install some basic packages

(straight-use-package 'dash)
(straight-use-package 'dash-functional)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'noflet)
(straight-use-package 'memoize)

(setq use-package-verbose t)

(straight-use-package 'bind-map)
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))


;; Load features.

(require 'paths (expand-file-name "paths.el" user-emacs-directory))
(paths-initialise)
(add-to-list 'custom-theme-load-path paths-themes-directory)

;; Load theme aggressively, or Emacs will look ugly during the startup sequence.
(use-package config-themes
  :config
  (config-themes/dark-theme))

(use-package config-basic-settings)
(use-package config-darwin :if (equal system-type 'darwin))
(use-package config-modeline)
(use-package config-editing)
(use-package config-hydras)
(use-package config-evil)
(use-package config-ivy)
(use-package config-navigation)
(use-package config-search)
(use-package config-projectile)
(use-package config-langs)
(use-package config-elisp)
(use-package config-smartparens)
(use-package config-git)
(use-package config-company)
(use-package config-yasnippet)
(use-package config-flycheck)
(use-package config-server)
(use-package config-neotree)
(use-package config-mu4e)
(use-package config-org)
(use-package config-ledger)
(use-package config-scala)
(use-package config-rust)
(use-package config-ibuffer)
(use-package config-dired)
(use-package config-web-mode)
(use-package config-markdown)
(use-package config-restclient)
(use-package config-haskell)
(use-package config-prodigy)
(use-package config-idris)
(use-package config-latex)
(use-package config-python)
(use-package config-eyebrowse)
(use-package config-nix)
(use-package config-fstar)
(use-package config-etags)

(use-package personal-config
  :load-path "~/Sync/emacs")

(unless user-full-name (warn "`user-full-name' not set"))
(unless user-mail-address (warn "`user-mail-address' not set"))


;;; Post init setup.

(unless (file-directory-p org-directory)
  (when (y-or-n-p (format "`org-directory' does not exist. Create at %s? " org-directory))
    (mkdir org-directory)))

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))


(provide 'init)

;;; init.el ends here
