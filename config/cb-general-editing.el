;;; cb-general-editing.el --- Configure general editing builtins  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)
(require 'general)

(general-def
  "M-t" 'transpose-words
  "M-SPC" 'cycle-spacing
  "C-c e e" 'toggle-debug-on-error
  "C-c e q" 'toggle-debug-on-quit)

(general-def :keymaps '(minibuffer-local-map
                        minibuffer-local-ns-map
                        minibuffer-local-completion-map
                        minibuffer-local-must-match-map
                        minibuffer-local-isearch-map)
  "<escape>" 'keyboard-escape-quit)

(general-unbind
  "<f2>" ; 2-window scrolling
  "S-<f2>" ; 2-window scrolling
  "C-z" ; suspend-frame
  "s-t" ; macOS font panel
  )

(use-package subr
  :config
  ;; Clean up whitespace when inserting yanked text
  (define-advice insert-for-yank (:after (&rest _))
    (whitespace-cleanup)
    (delete-trailing-whitespace)))

(use-package simple
  :config
  ;; Set reasonable default indentation settings
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil))

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package newcomment
  :preface
  (autoload 'thing-at-point-looking-at "thingatpt")
  (define-advice comment-indent-new-line (:after (&rest _) insert space)
    "Insert a leading space after comment start for new comment lines."
    (when (and comment-start
               (thing-at-point-looking-at (regexp-quote comment-start)))
      (unless (or (thing-at-point-looking-at (rx (+ space))))
        (just-one-space)))))

(use-package ffap
  :custom
  ;; Don't try to ping things that look like domain names.
  (ffap-machine-p-known 'reject))

(use-package files
  :custom
  ;; Don't confirm before killing subprocesses on exit.
  (confirm-kill-processes nil)
  :preface
  (define-advice save-buffers-kill-emacs (:around (fn &rest args) suppress-prompt)
    (cl-labels ((process-list () nil))
      (apply fn args)))

  ;; Convert ANSI color codes to text properties in shell output
  ;; (autoload 'ansi-color-apply-on-region "ansi-color")
  ;; (define-advice display-message-or-buffer (:before (buf &rest _) render-ansi)
  ;;   (and (bufferp buf)
  ;;        (string= (buffer-name buf) "*Shell Command Output*")
  ;;        (with-current-buffer buf
  ;;          (ansi-color-apply-on-region (point-min) (point-max)))))

  :custom
  ;; Share the Emacs kill ring with the host OS clipboard.
  (select-enable-clipboard t)
  (save-interprogram-paste-before-kill t)

  ;; Prevent duplicated entries in the kill ring
  (kill-do-not-save-duplicates t)

  ;; Do not truncate the results of `eval-expression'
  (eval-expression-print-length nil)
  (eval-expression-print-level nil))

(use-package pixel-scroll
  :demand t
  :config (pixel-scroll-mode +1))

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

(use-package autorevert
  :delight (auto-revert-mode " auto-revert")
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil))

;; Turns URLs and mailto links into clickable buttons
(use-package goto-addr
  :hook (prog-mode . goto-address-prog-mode))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  (advice-add 'hs-hide-all :around #'advice-ignore-errors)
  (advice-add 'hs-hide-block :around 'advice-ignore-errors)
  (advice-add 'hs-minor-mode :around #'advice-ignore-errors)
  (advice-add 'hs-show-all :around #'advice-ignore-errors)
  (advice-add 'hs-show-block :around #'advice-ignore-errors)
  (advice-add 'hs-toggle-hiding :around #'advice-ignore-errors))

(use-package compile
  :custom
  (compilation-environment '("TERM=screen-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package ansi-color
  :autoload (ansi-color-apply-on-region)
  :preface
  (defun cb-colourise-compilation-output ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (save-excursion
                                    (goto-char compilation-filter-start)
                                    (line-beginning-position))
                                  (point))))
  :hook (compilation-filter . cb-colourise-compilation-output))

(use-package ediff
  :custom
  ;; Configure how `ediff' should display windows when started.

  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)

  ;; Teach `ediff' how to copy contents from both buffers in a three-way merge.

  :autoload
  (ediff-setup-windows-plain ediff-copy-diff ediff-get-region-contents)

  :preface
  (defun cb-ediff-copy-both-to-C ()
    "Copy both ediff buffers in a 3-way merge to the target buffer."
    (interactive)
    (let ((str
           (concat
            (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
            (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
      (ediff-copy-diff ediff-current-difference nil 'C nil str)))

  :general
  (:keymaps 'ediff-mode-map "B" #'cb-ediff-copy-both-to-C)

  ;; Reveal the context around the selected hunk when diffing org buffers.

  :preface
  (autoload 'org-reveal "org")

  (defun cb--org-reveal-around-ediff-hunk (&rest _)
    (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (when (derived-mode-p 'org-mode)
            (org-reveal t))))))
  :config
  (advice-add 'ediff-next-difference :after #'cb--org-reveal-around-ediff-hunk)
  (advice-add 'ediff-previous-difference :after #'cb--org-reveal-around-ediff-hunk))

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))

(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))

(use-package align
  :general ("C-x a a" #'align-regexp))

(use-package hide-comnt
  :commands (hide/show-comments-toggle))

(use-package dumb-jump
  :autoload (dumb-jump-xref-activate)
  :custom
  (dumb-jump-selector 'completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "en_GB")
  (ispell-silently-savep t)
  :autoload (ispell-check-version ispell-find-aspell-dictionaries)
  :config
  (ispell-check-version)
  (setq ispell-dictionary-alist (ispell-find-aspell-dictionaries)))

(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  :custom
  (flyspell-issue-welcome-flag nil)
  (flyspell-default-dictionary "en_GB")
  :config
  (define-advice org-mode-flyspell-verify (:filter-return (result))
    (and result (not (equal (face-at-point nil t) 'org-link)))))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :general
  ("C-x t" 'undo-tree-visualize)
  (:states 'normal :keymaps 'org-mode-map
           "C-r" 'undo-tree-redo
           "u" 'undo-tree-undo))

(use-package format-all
  :hook
  (nix-mode . format-all-mode)
  (nix-mode . format-all-ensure-formatter)
  (terraform-mode . format-all-mode)
  (terraform-mode . format-all-ensure-formatter)
  :custom
  (format-all-show-errors 'never))

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-display-style 'image)
  (emojify-emoji-styles '(github))
  (emojify-program-contexts '(comments))
  (emojify-point-entered-behaviour 'uncover)
  :preface
  (cl-eval-when (compile)
    (require 'org))

  (defun emojify-at-org-drawer-p (&rest _)
    (when (derived-mode-p 'org-mode 'org-agenda-mode)
      (save-excursion
        (goto-char (line-beginning-position))
        (or (org-at-drawer-p) (org-at-property-p)))))

  :config
  (add-to-list 'emojify-inhibit-major-modes 'flymake-diagnostics-buffer-mode)
  (add-to-list 'emojify-inhibit-functions #'emojify-at-org-drawer-p)

  ;; Remove checkbox unicode emoji that conflicts with org-superstar.
  :preface
  (defun cb-emojify-modify-emojis ()
    (remhash "☑" emojify-emojis))
  :init
  (add-hook 'emojify-mode-hook #'cb-emojify-modify-emojis))

(use-package editorconfig
  :hook (after-init . editorconfig-mode)
  :preface
  (define-advice editorconfig--advice-insert-file-contents (:around (fn &rest args) handle-errors)
    (condition-case err
        (apply fn args)
      (file-missing
       nil)
      (error
       (throw (car err) (cdr err))))))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package rainbow-mode
  :hook
  (help-mode . rainbow-mode)
  (emacs-lisp-mode . rainbow-mode)
  (css-mode . rainbow-mode))

(use-package string-inflection
  :general ("M-s" 'string-inflection-all-cycle))

(use-package bufler
  :general
  ([remap list-buffers] 'bufler-list)
  :config
  (dolist (mode '(org-agenda-mode magit-status-mode))
    (add-to-list 'bufler-filter-buffer-modes mode)))

(use-package project
  :preface
  (autoload 'magit-git-repo-p "magit-git")
  (autoload 'magit-status "magit-status")

  (defun cb-project-switch ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (if (magit-git-repo-p dir)
          (magit-status dir)
        (dired dir))))
  :custom
  (project-vc-extra-root-markers '("Cargo.toml"
                                   "package.json"
                                   "flake.nix"
                                   "shell.nix"))
  (project-switch-commands 'cb-project-switch)
  (project-vc-ignores '(".direnv"
                        "cdk.out"
                        "node_modules"
                        "*.elc"
                        "*.eln"
                        "*.gz"
                        "*.meta"
                        "*.tar"
                        "*.tgz"
                        "*.zip")))



;; Use control key to transpose lines up and down.

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



(defun insert-uuid (&optional upcase-p)
  "Insert a UUID at point.

Prefix arg UPCASE-P determines whether the UUID is downcased or
upcased on insertion."
  (interactive "*P")
  (let ((uuid (string-trim (shell-command-to-string "uuidgen"))))
    (insert (if upcase-p (upcase uuid) (downcase uuid)))))

(defun insert-date (str)
  "Read date string STR interactively and insert it at point."
  (interactive (list
                (if (not current-prefix-arg)
                    (format-time-string "%F")
                  (let ((formats (seq-map #'format-time-string
                                          '("%F"
                                            "%F %R"
                                            "%X"
                                            "%c"))))
                    (completing-read "Format: " formats nil t)))))
  (insert str))



;;; Indentation

(require 'dash)
(autoload 'eglot-format "eglot")
(autoload 'eglot-managed-p "eglot")
(autoload 'format-all-buffer "format-all")

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
      (cond
       ((eglot-managed-p)
        (eglot-format (region-beginning) (region-end)))
       (t
        (indent-region (region-beginning) (region-end)))))
     (t
      (let ((progress (make-progress-reporter "Indenting buffer")))
        (cond
         ((bound-and-true-p format-all-mode)
          (format-all-buffer))
         ((eglot-managed-p)
          (eglot-format))
         (t
          ;; Inndent the entire buffer.
          (save-excursion
            (delete-trailing-whitespace)
            (indent-region (point-min) (point-max) nil)
            (untabify (point-min) (point-max)))))
        (progress-reporter-done progress))))))

(define-key prog-mode-map (kbd "M-q") #'cb-indent-dwim)

(provide 'cb-general-editing)

;;; cb-general-editing.el ends here
