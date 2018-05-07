;;; cb-basic-settings.el --- Basic Emacs settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 's)
(require 'subr-x)

(require 'cb-emacs)
(require 'spacemacs-keys)

(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'evil-define-key "evil-core")
(autoload 'evil-set-initial-state "evil-core")
(autoload 'evil-backward-char "evil-commands")


(defalias #'yes-or-no-p #'y-or-n-p)
(defalias #'view-hello-file #'ignore)

(global-unset-key (kbd "C-z"))


;; Make <escape> quit as much as possible

(define-key minibuffer-local-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") #'keyboard-escape-quit)

;; Write custom settings outside init.el

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Scroll smoothly.

(setq scroll-preserve-screen-position t)
(setq scroll-margin 0)
(setq scroll-conservatively 101)

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on RET
(define-key global-map (kbd "RET") #'comment-indent-new-line)

;; Disable backup files
(setq make-backup-files nil)

;; Enable hideshow in all programming buffers.

(autoload 'hs-minor-mode "hideshow")
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Make files executable on save.

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Show file or buffer name in the title bar.

(setq frame-title-format "Emacs")

;; Don't pollute directories with lockfiles, since I only run one instance of
;; Emacs and never need to prevent concurrent file access.
(setq create-lockfiles nil)

;; Copy system clipboard to the kill-ring if an Emacs kill would overwrite it.
(setq save-interprogram-paste-before-kill t)

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

(defalias 'indent-buffer #'cb-indent-buffer)

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

;; Evil breaks cursor settings when combined with hydra.

(setq-default cursor-in-non-selected-windows nil)

;; 2-window scrolling is never useful, but an emergency window switch command
;; sure is.

(global-set-key (kbd "<f2>") #'next-multiframe-window)

;; Unlimited print length for eval-expression.
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;;; Convenience aliases

(defalias 'bb  'bury-buffer)
(defalias 'hex 'hexl-mode)
(defalias 'hff 'hexl-find-file)
(defalias 'qr  'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'cal 'calendar)


;;; Saving behaviour

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable commands.

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;;; Core advice


;; Do not prompt for confirmation for active processes.

(defun cb-basic-settings--suppress-no-process-prompt (fn &rest args)
  (cl-labels ((process-list () nil))
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


;; Hide files with boring extensions from find-file

(defun cb-basic-settings--hide-boring-files-in-completion (result)
  "Filter RESULT using `completion-ignored-extensions'."
  (if (and (listp result) (stringp (car result)) (cdr result))
      (let ((matches-boring (rx-to-string `(and (or "."
                                                    ".."
                                                    ".DS_Store"
                                                    "__pycache__/"
                                                    ".cache/"
                                                    ".ensime_cache/"
                                                    ,@completion-ignored-extensions)
                                                eos))))
        (--remove (string-match-p matches-boring it) result))
    result))

(advice-add #'completion--file-name-table :filter-return #'cb-basic-settings--hide-boring-files-in-completion)


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

(global-set-key (kbd "s-<up>") #'cb-transpose-line-up)
(global-set-key (kbd "s-<down>") #'cb-transpose-line-down)

;;; Hide DOS EOL

(defun cb-basic-settings--hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook #'cb-basic-settings--hide-dos-eol)


;; Offer to open large files in fundamental mode.

(defvar cb-basic-settings--large-file-size (* 1024 1024)
  "Size in bytes above which a file is considered 'large'.")

(defconst cb-basic-settings--large-file-modes-list
  '(archive-mode
    doc-view-mode
    doc-view-mode-maybe
    ebrowse-tree-mode
    git-commit-mode
    image-mode
    pdf-view-mode
    tar-mode)
  "A list of modes that should not prompt for literal file editing.")

(defun cb-basic-settings--large-file? (size)
  (unless (memq major-mode cb-basic-settings--large-file-modes-list)
    (and size (> size cb-basic-settings--large-file-size))))

(defun cb-basic-settings--prompt-to-open-large-files-in-fundamental-mode ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename)))
         (tags-file? (when filename (equal "TAGS" (file-name-nondirectory filename)))))
    (when (and (cb-basic-settings--large-file? size)
               (not tags-file?)
               (not (seq-contains '("gz" "zip" "tar" "jar" "pdf") (file-name-extension filename)))
               (y-or-n-p (format "`%s' is a large file.  Open in fundamental mode? " (file-name-nondirectory filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(add-hook 'find-file-hook #'cb-basic-settings--prompt-to-open-large-files-in-fundamental-mode)


;;; General variables

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default sentence-end-double-space t)

(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash nil)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq initial-major-mode 'text-mode)
(setq initial-buffer-choice t)
(setq ring-bell-function #'ignore)
(setq history-length 1000)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; Display buffer customisations for inbuilt features

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Help*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . right)
               (slot            . 1)
               (window-width    . 0.5)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*compilation*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (slot            . 2)
               (window-width    . 0.3)))

;; Prevent display-buffer from displaying in new frames.

(setq display-buffer-fallback-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer-pop-up-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         cb-basic-settings-display-buffer-fallback)))

(defun cb-basic-settings-display-buffer-fallback (buffer &optional _alist)
  (-when-let (win (split-window-sensibly))
    (with-selected-window win
      (switch-to-buffer buffer)
      (help-window-setup (selected-window))))
  t)

;; Use conf mode for puppet templated conf files
(use-package conf-mode
  :mode
  (("\\.env\\.erb\\'" . conf-mode)
   ("\\.conf\\.erb\\'" . conf-mode)
   ("\\.kll\\'" . conf-mode)))

(use-package abbrev
  :defer t
  :config
  (setq abbrev-file-name (concat cb-emacs-cache-directory "/abbrev_defs")))

(use-package window-numbering
  :when (display-graphic-p)
  :defer t
  :commands (window-numbering-mode)
  :config
  (window-numbering-mode -1))

(use-package menu-bar
  :bind (("C-c e e" . toggle-debug-on-error))
  :if (bound-and-true-p menu-bar-mode)
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :if (bound-and-true-p tool-bar-mode)
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

    (defun cb-basic-settings-boring-filename-p (f)
      (memq (f-filename f)
            '("TAGS" ".DS_Store")))

    (defun cb-basic-settings-boring-extension-p (f)
      (seq-intersection (f-ext f)
                        '("gz" "zip" "tar")))

    (defun cb-basic-settings-child-of-boring-relative-dir-p (f)
      (seq-intersection (f-split f)
                        '(".git"
                          ".ensime_cache"
                          ".cargo"
                          ".stack-work"
                          ".g8"
                          "target"
                          "build"
                          "vendor"
                          "Maildir"
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
          '(cb-basic-settings-boring-filename-p
            cb-basic-settings-boring-extension-p
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
  :preface
  (progn
    (autoload 'ansi-color-apply-on-region "ansi-color")

    (defvar compilation-filter-start)

    (defun cb-basic-settings-colorize-compilation-buffer ()
      (unless (derived-mode-p 'rg-mode)
        (let ((inhibit-read-only t))
          (ansi-color-apply-on-region compilation-filter-start (point)))))

    (defface cb-compilation-base-face nil
      "Base face for compilation highlights"
      :group 'cb-basic-settings))

  :config
  (progn
    (add-hook 'compilation-filter-hook #'cb-basic-settings-colorize-compilation-buffer)

    ;; Clear default underline text properties applied to compilation highlights.
    (setq compilation-message-face 'cb-compilation-base-face)

    ;; h (help) binding interferes with evil navigation.
    (evil-define-key 'motion compilation-mode-map (kbd "h") #'evil-backward-char)

    (setq compilation-environment '("TERM=screen-256color"))
    (setq compilation-always-kill t)
    (setq compilation-ask-about-save nil)
    (setq compilation-scroll-output 'first-error)))

(use-package mule
  :preface
  (progn
    (defvar quail-current-package)

    (defun cb-basic-settings--echo-input-method ()
      (message (concat "Input method: " (propertize current-input-method 'face 'warning 'bold t))))

    (defun cb-basic-settings--echo-input-method-cleared ()
      (message (concat "Input method: " (propertize "cleared" 'face 'success))))

    (defun cb-basic-settings--set-tex-method-vars ()
      (when-let* ((quail-current-package (assoc "TeX" quail-package-alist)))
        (quail-defrule ";" (quail-lookup-key "\\"))
        (quail-define-rules ((append . t))
                            ("\\null" ?∅)
                            ("\\rarr" ?→)
                            ("\\larr" ?←)
                            ("\\lr" ?↔)
                            ("\\lam" ?λ)
                            ("\\Lam" ?Λ)
                            ("\\all" ?∀)
                            ("\\rtack" ?⊢)))))
  :config
  (progn
    (add-hook 'input-method-activate-hook #'cb-basic-settings--echo-input-method)
    (add-hook 'input-method-deactivate-hook #'cb-basic-settings--echo-input-method-cleared)
    (add-hook 'input-method-activate-hook #'cb-basic-settings--set-tex-method-vars)
    (setq default-input-method "TeX")))

(use-package comint
  :defer t
  :config
  (setq comint-prompt-read-only t))

(use-package hippie-exp
  :preface
  (autoload 'evil-global-set-key "evil-core")
  :init
  (progn
    (global-set-key (kbd "M-/") #'hippie-expand)
    (with-eval-after-load 'evil
      (evil-global-set-key 'insert [remap evil-complete-previous] #'hippie-expand)))

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

(use-package tramp
  :defer t
  :preface
  (progn
    (setq tramp-default-method "ssh")
    (setq tramp-auto-save-directory "/tmp")))

(use-package tramp-cache
  :defer t
  :config
  (setq tramp-persistency-file-name (concat cb-emacs-cache-directory "/tramp")))

(use-package autorevert
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)
    (global-auto-revert-mode 1)))

(use-package goto-addr
  :defer t
  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

(use-package ffap
  :defer t
  :config
  ;; Don't try to ping things that look like domain names
  (setq ffap-machine-p-known 'reject))

(use-package help
  :defer t
  :config
  ;; Always focus on help windows
  (setq help-window-select t))

(use-package world-time-mode
  :commands (world-time-list)
  :init
  (spacemacs-keys-set-leader-keys "aw" 'world-time-list)
  :config
  (progn
    (setq display-time-world-list '(("Pacific/Auckland" "NZT")
                                    ("UTC" "UTC")
                                    ("Europe/Berlin" "Germany")
                                    ("America/Los_Angeles" "Los Angeles")
                                    ("America/New_York" "New York")
                                    ("America/Denver" "Mountain Time")
                                    ("Australia/Sydney" "Sydney")))
    (with-eval-after-load 'evil
      (evil-define-key 'normal world-time-table-mode-map (kbd "q") #'quit-window))

    (add-hook 'world-time-table-mode-hook 'hl-line-mode))

  :defines (display-time-world-list world-time-table-mode-map))

(use-package hideshow
  :defer t
  :preface
  (defun cb-evil-ignore-errors (f &rest args)
    (ignore-errors
      (apply f args)))
  :config
  (dolist (cmd '(hs-minor-mode
                 hs-show-all
                 hs-hide-all
                 hs-toggle-hiding
                 hs-show-block
                 hs-hide-block))
    (advice-add cmd :around #'cb-evil-ignore-errors)))

(use-package apropos
  :defer t
  :config
  (setq apropos-do-all t))

(use-package archive-mode
  :defer t
  :preface
  (progn
    ;; HACK: `evil-set-initial-state' doesn't work with archive-mode. Run a hook
    ;; to set that instead.
    (defvar archive-mode-hook nil)

    (defun cb-basic-settings--run-archive-mode-hook ()
      (run-hooks 'archive-mode-hook)))

  :config
  (progn
    (advice-add 'archive-mode :after #'cb-basic-settings--run-archive-mode-hook)
    (add-hook 'archive-mode-hook 'evil-motion-state)

    (with-no-warnings
      (evil-define-key 'motion archive-mode-map
        (kbd "q") 'kill-this-buffer
        (kbd "RET") 'archive-extract
        (kbd "o") 'archive-extract-other-window
        (kbd "m") 'archive-mark
        (kbd "x") 'archive-expunge
        (kbd "U") 'archive-unmark-all-files
        (kbd "j") 'archive-next-line
        (kbd "k") 'archive-previous-line))))

(use-package doc-view
  :defer t
  :config
  (progn
    (setq doc-view-continuous t)
    (evil-set-initial-state 'doc-view-mode 'motion)
    (evil-define-key 'motion doc-view-mode-map
      (kbd "gg") 'doc-view-first-page
      (kbd "G") 'doc-view-last-page
      (kbd "j") 'doc-view-next-line-or-next-page
      (kbd "k") 'doc-view-previous-line-or-previous-page
      (kbd "h") 'image-backward-hscroll
      (kbd "l") 'image-forward-hscroll
      (kbd "n") 'doc-view-next-page
      (kbd "p") 'doc-view-previous-page
      (kbd "<down>") 'doc-view-next-line-or-next-page
      (kbd "<up>") 'doc-view-previous-line-or-previous-page
      (kbd "<left>") 'image-backward-hscroll
      (kbd "<right>") 'image-forward-hscroll)))

(use-package url-cookie
  :defer t
  :config
  (setq url-cookie-file (concat cb-emacs-cache-directory "/cookies")))

(use-package hide-comnt
  :commands (hide/show-comments-toggle)
  :init
  (spacemacs-keys-set-leader-keys "tc" #'hide/show-comments-toggle))

(use-package async
  :preface
  (progn
    (autoload 'async-bytecomp-package-mode "async-bytecomp")
    (autoload 'dired-async-mode "dired-async.el" nil t))
  :config
  (progn
    (async-bytecomp-package-mode +1)
    (dired-async-mode +1)))

(use-package url-cache
  :defer t
  :config
  (setq url-cache-directory (concat cb-emacs-cache-directory "/url")))

(use-package shr
  :defer t
  :config
  ;; Undefine key that prevents forward-word in evil
  (define-key shr-map (kbd "w") nil))

(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))

(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package nsm
  :defer t
  :config
  ;; Do not query the user to accept insecure connections. Just disconnect them.
  (setq nsm-noninteractive t))

(use-package page-break-lines
  :commands (global-page-break-lines-mode)
  :demand t
  :config
  (progn
    (setq page-break-lines-modes
          '(prog-mode
            text-mode
            compilation-mode
            help-mode
            org-agenda-mode))

    (global-page-break-lines-mode)))

(use-package generate-password
  :commands (generate-password))


(provide 'cb-basic-settings)

;;; cb-basic-settings.el ends here
