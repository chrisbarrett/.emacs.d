;;; early-init.el --- Lisp run early in init process  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst emacs-start-time (current-time))

(setenv "INSIDE_EMACS" "true")

;; Make sure package.el doesn't get a chance to load anything.

(setq package-enable-at-startup nil)

;; Defer garbage collection during startup

(setq gc-cons-threshold (* 512 1024 1024))
(setq gc-cons-percentage 0.7)

;; Crank up the max size of subprocess output to read.
(setq read-process-output-max (* 1024 1024))

;; disable some window chrome that doesn't make sense in a keyboard-driven UI.

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(modify-all-frames-parameters '((vertical-scroll-bars)))

(provide 'early-init)

;;; early-init.el ends here
