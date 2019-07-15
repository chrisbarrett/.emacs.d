;;; init.el --- Emacs configuration entrypoint  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "26")
  (error "This version of Emacs is not supported"))

(require 'subr-x)

(defconst emacs-start-time (current-time))

;; Allow overriding user-emacs-directory with envvar.
(setq user-emacs-directory (or (when-let* ((dir (getenv "USER_EMACS_DIRECTORY")))
                                 (if (string-suffix-p "/" dir) dir (concat dir "/")))
                               user-emacs-directory))

(setenv "INSIDE_EMACS" "true")

;; Make sure package.el doesn't get a chance to load anything.

(setq package-enable-at-startup nil)


;; Bootstrap straight.el package manager.

(eval-and-compile
  (defvar bootstrap-version 5)
  (defvar bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))

(unless (file-exists-p bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))

(load bootstrap-file nil 'nomessage)

(with-no-warnings
  (setq straight-cache-autoloads t)
  (setq straight-check-for-modifications '(watch-files)))

(require 'straight bootstrap-file t)


;; Install some basic packages

(with-no-warnings
  (setq use-package-verbose t))

(straight-use-package 'bind-map)
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(use-package dash :straight t)
(use-package dash-functional :straight t)
(use-package f :straight t)
(use-package s :straight t)
(use-package noflet :straight t)
(use-package memoize :straight t)
(use-package general :straight t :demand t)
(use-package el-patch :straight t)

(use-package hydra :straight t)

(use-package major-mode-hydra
  :straight t
  :demand t
  :general (:states '(normal motion) "," #'major-mode-hydra)
  :custom ((major-mode-hydra-invisible-quit-key "q")))


;; Load features.

(require 'paths (expand-file-name "paths.el" user-emacs-directory))
(paths-initialise)
(add-to-list 'custom-theme-load-path paths-themes-directory)

;; no-littering overrides many common paths to keep the .emacs.d directory
;; clean.
;;
;; Load it here since we want to refer to path vars, and need to make sure it's
;; loaded very early in the startup process.

(use-package no-littering
  :straight t
  :demand t
  :init
  (progn
    (setq no-littering-etc-directory paths-etc-directory)
    (setq no-littering-var-directory paths-cache-directory))
  :config
  (progn
    (setq auto-save-file-name-transforms
          `((".*" ,(expand-file-name "auto-save" paths-cache-directory) t)))

    (eval-when-compile
      (require 'recentf))

    (with-eval-after-load 'recentf
      (add-to-list 'recentf-exclude no-littering-etc-directory)
      (add-to-list 'recentf-exclude no-littering-var-directory))))


;; Load el-patch hacks.

(dolist (feature (seq-reduce (lambda (acc it)
                               (if (string-suffix-p ".el" it)
                                   (cons (intern (string-remove-suffix ".el" it))
                                         acc)
                                 acc))
                             (directory-files paths-hacks-directory)
                             nil))
  (eval `(use-package ,feature :demand t)))


;; Load OS-specific configuration.

(use-package config-darwin-os
  :if (equal system-type 'darwin)
  :demand t)


;; Load host-specific overrides for vars.

(when (file-exists-p paths-personal-config)
  (load-file paths-personal-config))

(when (file-exists-p paths-hostfile)
  (load-file paths-hostfile))


;; Load some config files first for consistency.

(use-package config-basic-settings)

;; Load remaining config files.

(dolist (feature (seq-reduce (lambda (acc it)
                               (if (and (string-suffix-p ".el" it)
                                        (not (string-suffix-p "-os.el" it)))
                                   (cons (intern (string-remove-suffix ".el" it))
                                         acc)
                                 acc))
                             (directory-files paths-config-directory)
                             nil))
  (eval `(use-package ,feature :demand t)))


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

;;; init.el ends here
