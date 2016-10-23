;;; cb-basic-settings.el --- Basic Emacs settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))


(autoload '-map "dash") ; needed for macro-expansion of `noflet'.
(require 'noflet)

(require 'cb-emacs)

(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'ansi-color-apply-on-region "ansi-color")


(defalias #'yes-or-no-p #'y-or-n-p)

(define-key minibuffer-local-map (kbd "ESC") #'keyboard-escape-quit)

(global-unset-key (kbd "C-z"))


;; Show file or buffer name in the title bar.

(defun cb-basic-settings--frame-title-string ()
  (if (buffer-file-name)
      (abbreviate-file-name (buffer-file-name))
    (buffer-name)))

(setq frame-title-format `(:eval (cb-basic-settings--frame-title-string)))


;;; Convenience aliases

(defalias 'bb  'bury-buffer)
(defalias 'hex 'hexl-mode)
(defalias 'hff 'hexl-find-file)
(defalias 'qr  'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'cal 'calendar)


;;; Saving behaviour

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;; Core advice


;; Do not prompt for confirmation for active processes.

(defun cb-basic-settings--suppress-no-process-prompt (fn &rest args)
  (noflet ((process-list () nil))
    (apply fn args)))

(advice-add #'save-buffers-kill-emacs :around #'cb-basic-settings--suppress-no-process-prompt)


;; Insert a leading space after comment start for new comment lines.

(defun cb-basic-settings--comment-insert-space ()
  (when (and comment-start
             (thing-at-point-looking-at (regexp-quote comment-start)))
    (unless (or (thing-at-point-looking-at (rx (+ space))))
      (just-one-space))))

(advice-add #'comment-indent-new-line :after #'cb-basic-settings--comment-insert-space)


;; Clean up whitespace when inserting yanked text.

(defun cb-basic-settings--yank-ws-cleanup (&rest _)
  (whitespace-cleanup)
  (delete-trailing-whitespace))

(advice-add #'insert-for-yank :after #'cb-basic-settings--yank-ws-cleanup)


;; Process ANSI color codes in shell output.

(defun cb-basic-settings--display-ansi-codes (buf &rest _)
  (and (bufferp buf)
       (string= (buffer-name buf) "*Shell Command Output*")
       (with-current-buffer buf
         (ansi-color-apply-on-region (point-min) (point-max)))))

(advice-add #'display-message-or-buffer :before #'cb-basic-settings--display-ansi-codes)


;;; Hide DOS EOL

(defun cb-basic-settings--hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook #'cb-basic-settings--hide-dos-eol)

;;; General variables

(setq-default fill-column 80)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash nil)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq initial-buffer-choice t)
(setq ring-bell-function #'ignore)

(use-package abbrev
  :defer t
  :config
  (setq abbrev-file-name (concat cb-emacs-cache-directory "abbrev_defs")))

(use-package window-numbering
  :defer t
  :commands (window-numbering-mode)
  :config
  (window-numbering-mode -1))

(use-package menu-bar
  :bind (("C-c e e" . toggle-debug-on-error))
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :if (display-graphic-p)
  :config
  (tool-bar-mode -1))

(use-package scroll-bar-mode
  :if (display-graphic-p)
  :config
  (scroll-bar-mode -1))

(use-package align
  :bind (("C-x a a" . align-regexp)))

(use-package simple
  :bind (("M-SPC" . cycle-spacing)))

(use-package recentf
  :defer t
  :config
  (progn
    (setq recentf-max-saved-items 1000)
    (setq recentf-save-file (concat cb-emacs-cache-directory "recentf"))))

(use-package bookmark
  :defer t
  :config
  (progn
    (setq bookmark-save-flag nil)
    (setq bookmark-default-file (concat cb-emacs-cache-directory "bookmarks"))))

(use-package files
  :config
  (progn
    (setq kept-new-versions 6)
    (setq require-final-newline t)
    (setq delete-old-versions t)
    (setq confirm-nonexistent-file-or-buffer nil)
    (setq backup-directory-alist `((".*" . ,cb-emacs-autosave-directory)))
    (setq version-control t)))

(use-package select
  :config
  (setq select-enable-clipboard t))

(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output 'first-error))

(use-package mule
  :config
  (setq default-input-method "TeX"))

(use-package comint
  :defer t
  :config
  (setq comint-prompt-read-only t))

(use-package hippie-exp
  :init
  (progn
    (global-set-key (kbd "M-/") #'hippie-expand)
    (with-eval-after-load 'evil
      (define-key evil-insert-state-map [remap evil-complete-previous] #'hippie-expand)))

  :config
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))


(provide 'cb-basic-settings)

;;; cb-basic-settings.el ends here
