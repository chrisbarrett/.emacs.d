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

;; Enable pixel-wise frame resizing so tiling window managers do the right
;; thing.

(setq frame-resize-pixelwise t)

;; Disable some window chrome that doesn't make sense in a keyboard-driven UI.

(custom-set-variables '(menu-bar-mode nil)
                      '(tool-bar-mode . nil)
                      '(scroll-bar-mode nil))

(modify-all-frames-parameters '((vertical-scroll-bars)
                                (name . "Emacs")))

(when (boundp 'comp-eln-load-path)
  (let ((cache-dir "~/.cache/emacs/eln-cache/"))
    (mkdir cache-dir t)
    (add-to-list 'comp-eln-load-path cache-dir)))

(provide 'early-init)

;;; early-init.el ends here
