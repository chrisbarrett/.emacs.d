;;; init.el --- Emacs configuration entrypoint  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "26")
  (error "This version of Emacs is not supported"))

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

(straight-use-package 'dash)
(straight-use-package 'dash-functional)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'memoize)
(straight-use-package 'general)
(straight-use-package 'el-patch)

(with-no-warnings
  (setq use-package-verbose t))

(straight-use-package 'bind-map)
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))



;;; init.el ends here
