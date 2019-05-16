;;; config-editing.el --- Config for simple editing packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'general)

;; Auto-indent on RET

(general-define-key :keymaps '(text-mode-map prog-mode-map)
  "RET" #'comment-indent-new-line)


;; Enable hideshow in all programming buffers.

(autoload 'hs-minor-mode "hideshow")
(add-hook 'prog-mode-hook 'hs-minor-mode)


;; Define line transposition commands.

(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")

(defun config-editing-transpose-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)

    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun config-editing-transpose-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)

    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

(global-set-key (kbd "C-<up>") #'config-editing-transpose-line-up)
(global-set-key (kbd "C-<down>") #'config-editing-transpose-line-down)

(global-set-key (kbd "s-<up>") #'config-editing-transpose-line-up)
(global-set-key (kbd "s-<down>") #'config-editing-transpose-line-down)



;; Define some editing commands.

;; Define a command to indent every line in the buffer. This should really be a
;; thing out-of-the-box.

(defun config-editing-indent-buffer ()
  "Indent the entire buffer."
  (interactive "*")
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(unless (fboundp 'indent-buffer)
  (defalias 'indent-buffer #'config-editing-indent-buffer))

(defun config-indent-dwim (&optional justify)
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
        (config-editing-indent-buffer)
        (progress-reporter-done progress))))))

(define-key prog-mode-map (kbd "M-q") #'config-indent-dwim)


;; Define a command for reversing the characters in the current region.

(defun config-reverse-characters (beg end)
  "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
  (interactive "*r")
  (insert
   (reverse
    (delete-and-extract-region
     beg end))))

(unless (fboundp 'reverse-characters)
  (defalias 'reverse-characters #'config-reverse-characters))



(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease default-text-scale-reset)
  :straight t)


;; Teach Emacs to respect editorconfig files.

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))


;; Configure conf-mode for use with more kinds of config files.

(use-package conf-mode
  :mode
  (("\\.env\\.erb\\'" . conf-mode)
   ("\\.conf\\.erb\\'" . conf-mode)
   ("\\.kll\\'" . conf-mode)))


;; Align provides useful functions for aligning text.

(use-package align
  :general ("C-x a a" #'align-regexp))


;; Hexl is Emacs' built-in hex editor.

(use-package hexl
  :defer t
  :general
  (:states 'motion :keymaps 'hexl-mode-map
   "]]" #'hexl-end-of-1k-page
   "[[" #'hexl-beginning-of-1k-page
   "h" #'hexl-backward-char
   "l" #'hexl-forward-char
   "j" #'hexl-next-line
   "k" #'hexl-previous-line
   "$" #'hexl-end-of-line
   "^" #'hexl-beginning-of-line
   "0" #'hexl-beginning-of-line))

;; aggressive-indent automatically reindents code during editing.

(use-package aggressive-indent
  :straight t
  :commands (global-aggressive-indent-mode)
  :hook (prog-mode . (lambda () (require 'aggressive-indent)))
  :init
  (general-setq aggressive-indent-excluded-modes
                '(csharp-mode
                  diff-auto-refine-mode
                  dockerfile-mode
                  fstar-mode
                  graphviz-dot-mode
                  haskell-mode
                  idris-mode
                  idris-repl-mode
                  inf-ruby-mode
                  java-mode
                  makefile-gmake-mode
                  makefile-mode
                  nix-mode
                  python-mode
                  restclient-mode
                  rust-mode
                  sql-mode
                  stylus-mode
                  terraform-mode
                  text-mode
                  toml-mode
                  web-js-mode
                  yaml-mode))

  :preface
  (defun turn-off-aggressive-indent-mode ()
    (when (fboundp 'aggressive-indent-mode)
      (aggressive-indent-mode -1)))

  :config
  (progn
    (add-hook 'diff-auto-refine-mode-hook #'turn-off-aggressive-indent-mode)
    (global-aggressive-indent-mode +1)))


;; volatile-highlights shows highlights in the buffer when regions change.

(use-package volatile-highlights
  :straight t
  :hook ((prog-mode . (lambda () (require 'volatile-highlights)))
         (text-mode . (lambda () (require 'volatile-highlights))))

  :commands (volatile-highlights-mode)
  :functions (vhl/install-extension
              vhl/define-extension
              vhl/load-extension)

  :config
  (progn

    ;; Evil support.

    (vhl/define-extension 'evil
                          'evil-move
                          'evil-paste-after
                          'evil-paste-before
                          'evil-paste-pop)

    (with-eval-after-load 'evil
      (vhl/install-extension 'evil)
      (vhl/load-extension 'evil))

    (volatile-highlights-mode)))


;; tiny provides a template syntax for creating sequences of text.

(use-package tiny
  :straight t
  :general ("C-:" #'tiny-expand))


;; ws-butler cleans up trailing whitespace as you edit.

(use-package ws-butler
  :straight t
  :commands (ws-butler-global-mode)
  :hook ((prog-mode . (lambda () (require 'ws-butler)))
         (text-mode . (lambda () (require 'ws-butler))))
  :config
  (ws-butler-global-mode))


;; unfill provides a command that is the opposite of fill-paragraph.

(use-package unfill
  :straight t
  :commands (unfill-region unfill-paragraph unfill-toggle))


;; hide-comnt provides a command for hiding comments.

(use-package hide-comnt
  :straight t
  :commands (hide/show-comments-toggle))


;; highlight-thing highlights the symbol at point.

(use-package highlight-thing
  :straight t
  :hook (prog-mode . highlight-thing-mode)
  :preface
  (progn
    (defun config-editing--face-ancestors (face)
      (let (result)
        (while (and face (not (equal face 'unspecified)))
          (setq result (cons face result))
          (setq face (face-attribute face :inherit)))
        (nreverse result)))

    (defun config-editing--should-highlight-p (res)
      (unless (bound-and-true-p lsp-ui-mode)
        (when res
          (let ((excluded-faces '(font-lock-string-face
                                  font-lock-keyword-face
                                  font-lock-comment-face
                                  font-lock-preprocessor-face
                                  font-lock-builtin-face))
                (faces (seq-mapcat #'config-editing--face-ancestors (face-at-point nil t))))
            (null (seq-intersection faces excluded-faces)))))))

  :config
  (progn
    (setq highlight-thing-what-thing 'symbol)
    (setq highlight-thing-delay-seconds 0.5)
    (setq highlight-thing-limit-to-defun nil)
    (setq highlight-thing-case-sensitive-p t)

    (advice-add 'highlight-thing-should-highlight-p :filter-return
                #'config-editing--should-highlight-p)))


;; auto-highlight-symbol highlights the symbol at point after a short time.

(use-package auto-highlight-symbol
  :straight t
  :hook (prog-mode . auto-highlight-symbol-mode)
  :preface
  ;; HACK: Prevent the default keymap from getting created
  (defvar auto-highlight-symbol-mode-map (make-sparse-keymap))
  :config
  (progn
    (setq ahs-case-fold-search nil)
    (setq ahs-default-range 'ahs-range-whole-buffer)
    (setq ahs-idle-interval 0.25)
    (setq ahs-inhibit-face-list nil)

    ;; Disable by default, use keybinding instead.
    (setq ahs-idle-timer 0)))


;; dump-jump provides a good fallback for navigating to definitions in the
;; absence of tags tables or semantic analysis.

(use-package dumb-jump
  :straight t
  :general (:states 'normal :keymaps 'prog-mode-map "M-." #'jump-to-definition)
  :config
  (progn
    (when (featurep 'ivy)
      (setq dumb-jump-selector 'ivy))

    (add-to-list 'dumb-jump-language-file-exts '(:language "javascript" :ext "ts" :agtype "js" :rgtype "ts"))))


;; autoinsert provides file templates.

(use-package autoinsert
  :preface
  (defvar auto-insert-alist nil)
  :hook (find-file . auto-insert)
  :config
  (setq auto-insert-query nil))

(use-package autoinsert-funcs
  :after autoinsert
  :config
  (dolist (form autoinsert-funcs-forms)
    (push form auto-insert-alist)))

;; ag provides integration with the `ag' program.

(use-package ag
  :straight t
  :commands ag)

;; rg provides integration with the `ripgrep' program.

(use-package rg
  :straight t
  :commands rg
  :config
  (setq rg-group-result t))

;; wgrep allows you to edit search results in a buffer then write those changes
;; to disk.

(use-package wgrep
  :straight t
  :commands (wgrep-setup)
  :general
  (:keymaps 'wgrep-mode-map [remap wgrep-finish-edit] #'config-editing--wgrep-finish-edit-kill-buffer)
  :init
  (add-hook 'grep-setup-hook #'wgrep-setup)
  :preface
  (progn
    (autoload 'wgrep-finish-edit "wgrep")

    (defun config-editing--wgrep-finish-edit-kill-buffer ()
      "Finish the current wgrep edit and kill the wgrep buffer."
      (interactive)
      (let ((buf (current-buffer)))
        (prog1 (wgrep-finish-edit)
          (kill-buffer buf)))))

  :config
  (progn
    (setq wgrep-enable-key (kbd "C-c C-e"))
    (setq wgrep-auto-save-buffer t)))

(provide 'config-editing)

;;; config-editing.el ends here
