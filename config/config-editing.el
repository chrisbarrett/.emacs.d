;;; config-editing.el --- Config for simple editing packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-major-mode-hydra)
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


;; Define a command for reversing the characters in the current region.

(defun cb-reverse-characters (beg end)
  "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
  (interactive "*r")
  (insert
   (reverse
    (delete-and-extract-region
     beg end))))

(defalias 'reverse-characters #'cb-reverse-characters)



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
  :init
  (cb-major-mode-hydra-define hexl-mode
    "Insert"
    (("d" hexl-insert-decimal-char "decimal")
     ("c" hexl-insert-octal-char "octal")
     ("x" hexl-insert-hex-char "hex")
     ("s" hexl-insert-hex-string "hex string"))

    "Address"
    (("g" hexl-goto-address "goto")))

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
                '(cb-web-js-mode
                  diff-auto-refine-mode
                  dockerfile-mode
                  fstar-mode
                  graphviz-dot-mode
                  haskell-mode
                  idris-mode
                  idris-repl-mode
                  inf-ruby-mode
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

    ;; Undo-tree support.

    (vhl/define-extension 'undo-tree
                          'undo-tree-redo
                          'undo-tree-undo
                          'undo-tree-move
                          'undo-tree-yank)

    (with-eval-after-load 'undo-tree
      (vhl/install-extension 'undo-tree)
      (vhl/load-extension 'undo-tree))

    (volatile-highlights-mode)))

;; tiny provides a template syntax for creating sequences of text.

(use-package tiny
  :straight t
  :general ("C-:" #'tiny-expand))

;; undo-tree visualises the undo history.

(use-package undo-tree
  :straight t
  :demand t
  :hook (org-mode . undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

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
      "Do not highlight symbol if looking at certain faces."
      (when res
        (let ((excluded-faces '(font-lock-string-face
                                font-lock-keyword-face
                                font-lock-comment-face
                                font-lock-preprocessor-face
                                font-lock-builtin-face))
              (faces (seq-mapcat #'config-editing--face-ancestors (face-at-point nil t))))
          (null (seq-intersection faces excluded-faces))))))

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

;; evil-ahs provides evil integration with highlight-symbol.

(use-package evil-ahs
  :commands (evil-ahs/highlight-symbol evil-ahs/goto-last-searched-symbol)
  :general (:states 'motion
            "*" #'evil-ahs/enter-ahs-forward
            "#" #'evil-ahs/enter-ahs-backward))

;; dump-jump provides a good fallback for navigating to definitions in the
;; absence of tags tables or semantic analysis.

(use-package dumb-jump
  :straight t
  :general (:states 'normal :keymaps 'prog-mode-map "M-." #'dumb-jump-go)
  :config
  (progn
    (setq dumb-jump-selector 'ivy)
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

(use-package js-autoinsert
  :after autoinsert
  :defer t
  :config
  (with-no-warnings
    (add-to-list 'auto-insert-alist
                 '((web-js-mode . "JavaScript") . js-autoinsert-template-string))))

(provide 'config-editing)

;;; config-editing.el ends here
