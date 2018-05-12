;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Declares some variables and bootstraps the rest of the configuration.
;;
;; One main difference from other configurations out there is that I use git subtrees for
;; many core packages, instead of relying on the Emacs package manager.

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

(defconst straight-cache-autoloads t)
(defconst straight-check-for-modifications 'live)

(require 'straight bootstrap-file t)


;; Install some basic packages

(straight-use-package 'dash)
(straight-use-package 'dash-functional)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'noflet)
(straight-use-package 'memoize)

(defconst use-package-verbose t)

(straight-use-package 'bind-map)
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))


;; Load features.

(eval-and-compile
  (defvar cb-paths-directory
    (expand-file-name "config" user-emacs-directory)))

(use-package cb-paths
  :demand t
  :load-path cb-paths-directory
  :config
  (progn
    (cb-paths-initialise)
    (add-to-list 'custom-theme-load-path cb-paths-themes-directory)))

(use-package cb-themes)
(use-package cb-basic-settings)
(use-package cb-modeline)
(use-package cb-auto-save)
(use-package cb-leader-keys)
(use-package cb-evil)
(use-package cb-ivy)
(use-package cb-avy)
(use-package cb-darwin :if (equal system-type 'darwin))
(use-package cb-dumb-jump)
(use-package cb-search)
(use-package cb-projectile)
(use-package cb-elisp)
(use-package cb-smartparens)
(use-package cb-git)
(use-package cb-company)
(use-package cb-undo-tree)
(use-package cb-ws-butler)
(use-package cb-yasnippet)
(use-package cb-parentheses)
(use-package cb-flycheck)
(use-package cb-aggressive-indent)
(use-package cb-server)
(use-package cb-hexl)
(use-package cb-volatile-highlights)
(use-package cb-info)
(use-package cb-highlight-todo)
(use-package cb-neotree)
(use-package cb-mu4e)
(use-package cb-ahs)
(use-package cb-org)
(use-package cb-ledger)
(use-package cb-scala)
(use-package cb-groovy)
(use-package cb-rust)
(use-package cb-ibuffer)
(use-package cb-yaml)
(use-package cb-go)
(use-package cb-dired)
(use-package cb-spelling)
(use-package cb-diff)
(use-package cb-coffeescript)
(use-package cb-web-mode)
(use-package cb-markdown)
(use-package cb-restclient)
(use-package cb-calc)
(use-package cb-haskell)
(use-package cb-csv)
(use-package cb-highlight-thing)
(use-package cb-idris)
(use-package cb-docker)
(use-package cb-graphviz)
(use-package cb-protobuf)
(use-package cb-latex)
(use-package cb-python)
(use-package cb-gud)
(use-package cb-eyebrowse)
(use-package cb-nix)
(use-package cb-tiny)
(use-package cb-fstar)
(use-package cb-terraform)
(use-package cb-manpages)
(use-package cb-etags)

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
