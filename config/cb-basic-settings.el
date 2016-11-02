;;; cb-basic-settings.el --- Basic Emacs settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 's)
(require 'noflet)

(require 'cb-emacs)

(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'ansi-color-apply-on-region "ansi-color")


(defalias #'yes-or-no-p #'y-or-n-p)

(global-unset-key (kbd "C-z"))


;; Make <escape> quit as much as possible

(define-key minibuffer-local-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") #'keyboard-escape-quit)

;; Write custom settings outside init.el

(setq custom-file (concat user-emacs-directory "custom.el"))

;; Scroll smoothly.

(setq scroll-preserve-screen-position t)
(setq scroll-margin 0)
(setq scroll-conservatively 101)

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on RET
(define-key global-map (kbd "RET") #'comment-indent-new-line)


;; Enable hideshow in all programming buffers.

(autoload 'hs-minor-mode "hideshow")
(add-hook 'prog-mode-hook 'hs-minor-mode)


;; Show file or buffer name in the title bar.

(defun cb-basic-settings--frame-title-string ()
  (if (buffer-file-name)
      (abbreviate-file-name (buffer-file-name))
    (buffer-name)))

(setq frame-title-format `(:eval (cb-basic-settings--frame-title-string)))

;; Don't prompt when following symlinks to vc files.
(setq vc-follow-symlinks t)

;; This should really be a thing out-of-the-box.

(defun cb-indent-buffer ()
  "Indent the entire buffer."
  (interactive "*")
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun cb-indent-dwim (&optional justify)
  "Indent the thing at point.

Knows how to fill strings and comments, or indent code.

Optional arg JUSTIFY will justify comments and strings."
  (interactive "*P")
  (-let [(_ _ _ string-p comment-p) (syntax-ppss)]
    (cond
     (string-p
      (let ((progress (make-progress-reporter "Filling paragraph")))
        (fill-paragraph justify)
        (progress-reporter-done progress)))
     (comment-p
      (let ((progress (make-progress-reporter "Filling comment")))
        (fill-comment-paragraph justify)
        (progress-reporter-done progress)))
     ((region-active-p)
      (indent-region (region-beginning) (region-end)))
     (t
      (let ((progress (make-progress-reporter "Indenting buffer")))
        (cb-indent-buffer)
        (progress-reporter-done progress))))))

(define-key prog-mode-map (kbd "M-q") #'cb-indent-dwim)


;; 2-window scrolling is never useful, but an emergency window switch command
;; sure is.

(global-set-key (kbd "<f2>") #'other-window)


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

(defun cb-basic-settings--comment-insert-space (&rest _)
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


;; Line transposition

(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")

(defun cb-transpose-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)

    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun cb-transpose-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)

    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

(global-set-key (kbd "C-<up>") #'cb-transpose-line-up)
(global-set-key (kbd "C-<down>") #'cb-transpose-line-down)


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
(setq inhibit-startup-message t)
(setq initial-major-mode 'text-mode)
(setq initial-buffer-choice t)
(setq ring-bell-function #'ignore)

(use-package abbrev
  :defer t
  :config
  (setq abbrev-file-name (concat cb-emacs-cache-directory "/abbrev_defs")))

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

(use-package scroll-bar
  :if (display-graphic-p)
  :config
  (scroll-bar-mode -1))

(use-package align
  :bind (("C-x a a" . align-regexp)))

(use-package simple
  :bind (("M-SPC" . cycle-spacing)))

(use-package recentf
  :defer t
  :preface
  (progn
    (defun cb-basic-settings-boring-extension-p (f)
      (seq-intersection (f-ext f)
                        '("gz" "zip" "tar")))

    (defun cb-basic-settings-child-of-boring-relative-dir-p (f)
      (seq-intersection (f-split f)
                        '(".git"
                          ".ensime_cache"
                          ".cargo"
                          ".stack_work"
                          "target"
                          "build"
                          "dist")))

    (defun cb-basic-settings-child-of-boring-abs-dir-p (f)
      (let ((ignore-case (eq system-type 'darwin)))
        (seq-find (lambda (d)
                    (s-starts-with? d f ignore-case))
                  (list "/var/folders/"
                        "/usr/local/Cellar/"
                        "/tmp/"
                        (f-expand (concat user-emacs-directory "snippets/")))))))

  :config
  (progn
    (setq recentf-max-saved-items 1000)
    (setq recentf-save-file (concat cb-emacs-cache-directory "/recentf"))
    (setq recentf-exclude
          '(cb-basic-settings-boring-extension-p
            cb-basic-settings-child-of-boring-relative-dir-p
            cb-basic-settings-child-of-boring-abs-dir-p))))

(use-package bookmark
  :defer t
  :config
  (progn
    (setq bookmark-save-flag nil)
    (setq bookmark-default-file (concat cb-emacs-cache-directory "/bookmarks"))))

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

(use-package winner
  :preface
  (defvar cb-basic-settings-winner-boring-buffers
    '("*Completions*"
      "*Compile-Log*"
      "*inferior-lisp*"
      "*Fuzzy Completions*"
      "*Apropos*"
      "*Help*"
      "*cvs*"
      "*Buffer List*"
      "*Ibuffer*"
      "*esh command on file*"))
  :config
  (progn
    (winner-mode t)
    (setq winner-boring-buffers (append winner-boring-buffers cb-basic-settings-winner-boring-buffers))))

(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat cb-emacs-cache-directory "/saveplace"))
    (save-place-mode +1)))

(use-package savehist
  :init
  (defconst savehist-file (concat cb-emacs-cache-directory "/savehist"))
  :config
  (progn
    (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
    (savehist-mode +1)))


(provide 'cb-basic-settings)

;;; cb-basic-settings.el ends here
