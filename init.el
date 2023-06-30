;;; init.el --- User init file for Emacs  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar org-roam-index-node-id)



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

(when (file-exists-p custom-file)
  (load custom-file t t))

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

(use-package mule-cmds
  :config
  ;; Use UTF-8 everywhere
  ;; See:
  ;; https://www.reddit.com/r/emacs/comments/siuvpu/comment/hvbns5f
  (set-language-environment "UTF-8")
  (set-locale-environment "en_NZ.UTF-8"))

;; Do not truncate the results of `eval-expression'
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; Instantly display current keystrokes in minibuffer
(setq echo-keystrokes 0.02)

;; Enable useful commands that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Set global keybindings for `toggle-debug-on-error' and friends
(general-define-key "C-c e e" 'toggle-debug-on-error)
(general-define-key "C-c e q" 'toggle-debug-on-quit)

;; Set reasonable default indentation settings
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

;; Make scripts executable after save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Don't require two spaces to signal the end of a sentence. I don't use
;; sentence-based commands that often anyway.
(setq sentence-end-double-space nil)

;; Insert a leading space after comment start for new comment lines.
(autoload 'thing-at-point-looking-at "thingatpt")
(define-advice comment-indent-new-line (:after (&rest _) insert space)
  (when (and comment-start
             (thing-at-point-looking-at (regexp-quote comment-start)))
    (unless (or (thing-at-point-looking-at (rx (+ space))))
      (just-one-space))))

(general-define-key "M-SPC" 'cycle-spacing)

;; Disable cursor blinking.
(blink-cursor-mode -1)

;; Unset 2-window scrolling shortcuts.
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "S-<f2>"))

(use-package ffap
  :custom
  ;; Don't try to ping things that look like domain names.
  (ffap-machine-p-known 'reject))

;; Don't confirm before killing subprocesses on exit.
(setq confirm-kill-processes nil)
(define-advice save-buffers-kill-emacs (:around (fn &rest args) suppress-prompt)
  (cl-labels ((process-list () nil))
    (apply fn args)))

;; Disable suspend-frame on C-z
(global-unset-key (kbd "C-z"))

;; Increase minibuffer history length.
(setq history-length 1000)

;; Hide boring files from find-file completion candidates.
(define-advice completion--file-name-table (:filter-return (result) remove-boring-files)
  (if (and (listp result) (stringp (car result)) (cdr result))
      (let ((matches-boring (rx-to-string `(and (or "." ".." ".DS_Store" ,@completion-ignored-extensions) eos))))
        (seq-remove (lambda (it)
                      (and (stringp it) (string-match-p matches-boring it)))
                    result))
    result))

;; Remove any lingering *completions* buffer on minibuffer exit
(defun cb--cleanup-completions-buffer ()
  (when-let* ((buf (get-buffer "*Completions*")))
    (kill-buffer buf)))
(add-hook 'minibuffer-exit-hook #'cb--cleanup-completions-buffer)

;; Share the Emacs kill ring with the host OS clipboard.
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

;; Prevent duplicated entries in the kill ring
(setq kill-do-not-save-duplicates t)

;; Clean up whitespace when inserting yanked text
(define-advice insert-for-yank (:after (&rest _))
  (whitespace-cleanup)
  (delete-trailing-whitespace))

;; Anchor the cursor to the top or bottom of the window during scrolling, rather
;; than paginating through the buffer.
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 101)

;; Disable font-panel keybinding in macOS.
(global-set-key (kbd "s-t") nil)

;; Always focus on help windows
(setq help-window-select t)

;; Don't show 'press q to close' message
(advice-add 'help-window-display-message :override #'ignore)

(use-package comint
  :custom
  (comint-prompt-read-only t))

(use-package apropos
  :custom
  (apropos-do-all t))

(use-package saveplace
  :demand t
  :config (save-place-mode +1))

(use-package savehist
  :demand t
  :config (savehist-mode +1)
  :custom
  (savehist-additional-variables '(kill-ring
                                   compile-command
                                   search-ring
                                   regexp-search-ring)))

;; Configure Emacs so that each paragraph may have a difference text direction.
(setq-default bidi-paragraph-separate-re "^")
(setq-default bidi-paragraph-start-re "^")

;;; init.el ends here
