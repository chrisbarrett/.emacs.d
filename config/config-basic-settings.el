;;; config-basic-settings.el --- Basic Emacs settings.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'no-littering)
(require 'paths)

(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'thing-at-point-looking-at "thingatpt")


;; Ensure we always use one-char y-or-n-p.

(defalias #'yes-or-no-p #'y-or-n-p)

;; Never show the useless hello file.

(defalias #'view-hello-file #'ignore)


;; Write custom settings outside init.el

(general-setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load-file custom-file))


;; 2-window scrolling is never useful, but an emergency window switch command
;; sure is.

(global-set-key (kbd "<f2>") #'next-multiframe-window)
(global-set-key (kbd "S-<f2>") #'previous-multiframe-window)


;;; Define some convenience aliases

(defalias 'bb  'bury-buffer)
(defalias 'hex 'hexl-mode)
(defalias 'hff 'hexl-find-file)
(defalias 'qr  'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'cal 'calendar)


;;; Saving behaviour

(global-set-key (kbd "<f9>") #'save-buffer)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)


;; Enable commands.

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;; Use UTF-8 everywhere.

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)


;; General settings.

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

(general-setq
 sentence-end-double-space nil
 delete-by-moving-to-trash nil
 initial-scratch-message nil
 inhibit-startup-message t
 initial-major-mode 'fundamental-mode
 ring-bell-function #'ignore
 history-length 1000

 kept-new-versions 6
 require-final-newline t
 delete-old-versions t
 confirm-nonexistent-file-or-buffer nil
 version-control t
 select-enable-clipboard t
 tramp-default-method "ssh"

 ;; Do not truncate the results of `eval-expression' and friends when logging
 ;; their output with `message'.
 eval-expression-print-length nil
 eval-expression-print-level nil

 frame-title-format (unless (equal system-type 'darwin) "Emacs")

 ;; Scroll smoothly.
 scroll-preserve-screen-position t
 scroll-margin 0
 scroll-conservatively 101

 ;; Instantly display current keystrokes in mini buffer
 echo-keystrokes 0.02

 ;; Disable backup files
 make-backup-files nil
 backup-directory-alist (list (cons ".*" (no-littering-expand-var-file-name "backup/")))

 ;; Don't pollute directories with lockfiles, since I only run one instance of
 ;; Emacs and never need to prevent concurrent file access.
 create-lockfiles nil

 ;; Copy system clipboard to the kill-ring if an Emacs kill would overwrite it.
 save-interprogram-paste-before-kill t

 ;; Don't prompt when following symlinks to vc files.
 vc-follow-symlinks t

 ;; Don't try to ping things that look like domain names
 ffap-machine-p-known 'reject

 ;; Disable warnings from obsolete advice system, since these are generally not
 ;; actionable.
 ad-redefinition-action 'accept

 compilation-environment '("TERM=screen-256color")
 compilation-always-kill t
 compilation-ask-about-save nil
 compilation-scroll-output 'first-error

 comint-prompt-read-only t

 ;; Always focus on help windows
 help-window-select t

 ;; Prevent duplicated entries in the kill ring.
 kill-do-not-save-duplicates t

 apropos-do-all t

 ;; Don't confirm before killing subprocesses on exit.
 confirm-kill-processes nil

 hippie-expand-try-functions-list
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
   try-complete-lisp-symbol)

 winner-boring-buffers
 '("*Completions*"
   "*Compile-Log*"
   "*inferior-lisp*"
   "*Fuzzy Completions*"
   "*Apropos*"
   "*Help*"
   "*cvs*"
   "*Buffer List*"
   "*Ibuffer*"
   "*esh command on file*")

 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 savehist-additional-variables '(kill-ring search-ring regexp-search-ring)

 url-cookie-file (concat paths-cache-directory "/cookies")

 doc-view-continuous t

 ;; Do not query the user to accept insecure connections. Just disconnect them.
 nsm-noninteractive t
 )

;; Make <escape> issue a keyboard-quit in as many situations as possible.

(define-key minibuffer-local-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") #'keyboard-escape-quit)

;; Evil breaks cursor settings when combined with hydra.
(setq-default cursor-in-non-selected-windows nil)

;; Bind a key command to toggle side windows.
(global-set-key (kbd "C-c TAB") #'window-toggle-side-windows)

(setq-default bidi-paragraph-separate-re "^")
(setq-default bidi-paragraph-start-re "^")


;;; Core advice

;; Don't show 'press q to close' message
(advice-add 'help-window-display-message :override #'ignore)


;; Do not prompt for confirmation for active processes.

(defun config-basic-settings--suppress-no-process-prompt (fn &rest args)
  (cl-labels ((process-list () nil))
    (apply fn args)))

(advice-add #'save-buffers-kill-emacs :around #'config-basic-settings--suppress-no-process-prompt)


;; Insert a leading space after comment start for new comment lines.

(defun config-basic-settings--comment-insert-space (&rest _)
  (when (and comment-start
             (thing-at-point-looking-at (regexp-quote comment-start)))
    (unless (or (thing-at-point-looking-at (rx (+ space))))
      (just-one-space))))

(advice-add #'comment-indent-new-line :after #'config-basic-settings--comment-insert-space)


;; Clean up whitespace when inserting yanked text.

(defun config-basic-settings--yank-ws-cleanup (&rest _)
  (whitespace-cleanup)
  (delete-trailing-whitespace))

(advice-add #'insert-for-yank :after #'config-basic-settings--yank-ws-cleanup)


;; Process ANSI color codes in shell output.

(defun config-basic-settings--display-ansi-codes (buf &rest _)
  (and (bufferp buf)
       (string= (buffer-name buf) "*Shell Command Output*")
       (with-current-buffer buf
         (ansi-color-apply-on-region (point-min) (point-max)))))

(advice-add #'display-message-or-buffer :before #'config-basic-settings--display-ansi-codes)


;; Hide files with boring extensions from find-file

(defun config-basic-settings--hide-boring-files-in-completion (result)
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
        (seq-remove (lambda (it)
                      (and (stringp it) (string-match-p matches-boring it)))
                    result))
    result))

(advice-add #'completion--file-name-table :filter-return #'config-basic-settings--hide-boring-files-in-completion)


;;; Hide DOS EOL characters.

(defun config-basic-settings--hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook #'config-basic-settings--hide-dos-eol)


;; Offer to open large files in fundamental mode.

(defvar config-basic-settings--large-file-size (* 1024 1024)
  "Size in bytes above which a file is considered 'large'.")

(defconst config-basic-settings--large-file-modes-list
  '(archive-mode
    doc-view-mode
    doc-view-mode-maybe
    ebrowse-tree-mode
    git-commit-mode
    image-mode
    pdf-view-mode
    tar-mode)
  "A list of modes that should not prompt for literal file editing.")

(defun config-basic-settings--large-file? (size)
  (unless (memq major-mode config-basic-settings--large-file-modes-list)
    (and size (> size config-basic-settings--large-file-size))))

(defun config-basic-settings--prompt-to-open-large-files-in-fundamental-mode ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename)))
         (tags-file? (when filename (equal "TAGS" (file-name-nondirectory filename)))))
    (when (and (config-basic-settings--large-file? size)
               (not tags-file?)
               (not (seq-contains '("gz" "zip" "tar" "jar" "pdf") (file-name-extension filename)))
               (y-or-n-p (format "`%s' is a large file.  Open in fundamental mode? " (file-name-nondirectory filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(add-hook 'find-file-hook #'config-basic-settings--prompt-to-open-large-files-in-fundamental-mode)


;; Clean up completions buffers

(defun config-basic-settings--cleanup-completions-buffer ()
  (when-let* ((buf (get-buffer "*Completions*")))
    (kill-buffer buf)))

(add-hook 'minibuffer-exit-hook #'config-basic-settings--cleanup-completions-buffer)



;; Prevent display-buffer from displaying in new frames.

(defun config-basic-settings--display-buffer-fallback (buffer &rest _)
  (when-let* ((win (split-window-sensibly)))
    (with-selected-window win
      (switch-to-buffer buffer)
      (help-window-setup (selected-window))))
  t)

(general-setq
 display-buffer-fallback-action
 '((display-buffer--maybe-same-window
    display-buffer-reuse-window
    display-buffer-pop-up-window
    display-buffer-in-previous-window
    display-buffer-use-some-window
    config-basic-settings--display-buffer-fallback)))



(use-package simple
  :general ("M-SPC" #'cycle-spacing))


(use-package recentf
  :defer t
  :preface
  (progn

    (defun config-basic-settings--boring-filename-p (f)
      (memq (f-filename f) '("TAGS" ".DS_Store")))

    (defun config-basic-settings--boring-extension-p (f)
      (seq-intersection (f-ext f) '("gz" "zip" "tar")))

    (defun config-basic-settings--sudo-file-p (f)
      (string-prefix-p "/sudo:root@" f))

    (defun config-basic-settings--child-of-boring-relative-dir-p (f)
      (string-match-p (rx "/"
                          (or
                           ".cargo"
                           ".ensime_cache"
                           ".g8"
                           ".git"
                           ".stack-work"
                           "Maildir"
                           "build"
                           "dist"
                           "flow-typed/npm"
                           "straight/repos"
                           "target"
                           "vendor"
                           )
                          "/")
                      f))

    (defconst config-basic-settings--abs-dirs
      (seq-map (lambda (it) (f-slash (file-truename it)))
               (list "/var/folders/"
                     "/usr/local/Cellar/"
                     "/tmp/"
                     "/nix/store/"
                     paths-cache-directory
                     paths-etc-directory)))

    (defun config-basic-settings--child-of-boring-abs-dir-p (f)
      (let ((ignore-case (eq system-type 'darwin)))
        (seq-find (lambda (d)
                    (or
                     (string-prefix-p d f ignore-case)
                     (string-prefix-p d (file-truename f) ignore-case)))
                  config-basic-settings--abs-dirs))))

  :config
  (general-setq
   recentf-max-saved-items 1000
   recentf-exclude '(config-basic-settings--boring-filename-p
                     config-basic-settings--boring-extension-p
                     file-remote-p
                     config-basic-settings--sudo-file-p
                     config-basic-settings--child-of-boring-relative-dir-p
                     config-basic-settings--child-of-boring-abs-dir-p)))


(use-package compile
  :defer t
  :preface
  (progn
    (autoload 'ansi-color-apply-on-region "ansi-color")

    (defvar compilation-filter-start)

    (defun config-basic-settings--colorize-compilation-buffer ()
      (unless (derived-mode-p 'rg-mode)
        (let ((inhibit-read-only t))
          (ansi-color-apply-on-region compilation-filter-start (point)))))

    (defface compilation-base-face nil
      "Base face for compilation highlights"
      :group 'config-basic-settings))

  :config
  (progn
    (add-hook 'compilation-filter-hook #'config-basic-settings--colorize-compilation-buffer)

    ;; Clear default underline text properties applied to compilation highlights.
    (setq compilation-message-face 'compilation-base-face)))


(use-package mule
  :preface
  (progn
    (defvar quail-current-package)

    (defun config-basic-settings--set-tex-method-vars ()
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
  (add-hook 'input-method-activate-hook #'config-basic-settings--set-tex-method-vars))


(use-package hippie-exp
  :general ("M-/" 'hippie-expand
            :states 'insert
            [remap evil-complete-previous] 'hippie-expand))


(use-package winner
  :general ("<C-left>" 'winner-undo
            "<C-right>"'winner-redo)
  :config (winner-mode t))


(use-package saveplace
  :config (save-place-mode +1))


(use-package savehist
  :config (savehist-mode +1))


(use-package autorevert
  :config (global-auto-revert-mode 1))

(use-package goto-addr
  :hook (prog-mode . goto-address-prog-mode))

(use-package world-time-mode
  :straight t
  :commands (world-time-list)
  :general
  (:states 'normal :keymaps 'world-time-table-mode-map "q" 'quit-window)
  :config
  (progn
    (setq display-time-world-list '(("Pacific/Auckland" "NZT")
                                    ("America/Los_Angeles" "Pacific Time")
                                    ("Europe/Istanbul" "Turkey")
                                    ("Asia/Beirut" "Lebanon")
                                    ("Europe/Berlin" "Euro Central")
                                    ("UTC" "UTC")))

    (add-hook 'world-time-table-mode-hook 'hl-line-mode)))


(use-package hideshow
  :defer t
  :preface
  (defun config-basic-settings--ignore-errors (f &rest args)
    (ignore-errors
      (apply f args)))
  :config
  (dolist (cmd '(hs-minor-mode
                 hs-show-all
                 hs-hide-all
                 hs-toggle-hiding
                 hs-show-block
                 hs-hide-block))
    (advice-add cmd :around #'config-basic-settings--ignore-errors)))


(use-package archive-mode
  :defer t
  :preface
  (progn
    ;; KLUDGE: archive-mode doesn't run a hook.
    (defvar archive-mode-hook nil)
    (defun config-basic-settings--run-archive-mode-hook ()
      (run-hooks 'archive-mode-hook)))
  :config
  (advice-add 'archive-mode :after #'config-basic-settings--run-archive-mode-hook))


(use-package async
  :straight t
  :preface
  (progn
    (autoload 'async-bytecomp-package-mode "async-bytecomp")
    (autoload 'dired-async-mode "dired-async.el" nil t))
  :config
  (progn
    (async-bytecomp-package-mode +1)
    (dired-async-mode +1)))


(use-package shr
  :defer t
  :config
  ;; Undefine key that prevents forward-word in evil
  (define-key shr-map (kbd "w") nil))


(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg")))


(use-package pixel-scroll
  :demand t
  :config (pixel-scroll-mode +1))


(use-package term
  :commands (ansi-term)
  :preface
  (defun config-basic-settings--shell-hl-line-off ()
    (when (bound-and-true-p hl-line-mode)
      (hl-line-mode -1)))
  :config
  (add-hook 'term-mode-hook #'config-basic-settings--shell-hl-line-off))


(use-package calc
  :commands (quick-calc calc))

(use-package man
  :defer t
  :general (:keymaps 'Man-mode-map
            "M-n" #'Man-next-section
            "M-p" #'Man-previous-section))


(use-package ispell
  :defer t
  :preface
  (progn
    (autoload 'ispell-check-version "ispell")
    (autoload 'ispell-find-aspell-dictionaries "ispell"))
  :config
  (progn
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "en_GB")
    (ispell-check-version)
    (setq ispell-dictionary-alist (ispell-find-aspell-dictionaries))
    (setq ispell-silently-savep t)))


(use-package info+
  :after info
  :straight (:host github :repo "emacsmirror/info-plus")
  :defer t
  :config
  (general-setq Info-fontify-angle-bracketed-flag nil))


(use-package direnv
  :straight t
  :config
  (progn
    (general-setq direnv-always-show-summary nil)
    (direnv-mode)))


(use-package ediff
  :defer t
  :preface
  (progn
    (autoload 'ediff-setup-windows-plain "ediff-wind")
    (autoload 'ediff-copy-diff "ediff-util")
    (autoload 'ediff-get-region-contents "ediff-util")

    (defun ediff-copy-both-to-C ()
      (interactive)
      (let ((str
             (concat
              (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
              (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
        (ediff-copy-diff ediff-current-difference nil 'C nil str)))

    (defun config-basic-settings--setup-ediff-keybinds ()
      (define-key ediff-mode-map (kbd "B") #'ediff-copy-both-to-C)))
  :config
  (progn
    (add-hook 'ediff-keymap-setup-hook #'config-basic-settings--setup-ediff-keybinds)
    (setq ediff-window-setup-function #'ediff-setup-windows-plain)))


(provide 'config-basic-settings)

;;; config-basic-settings.el ends here
