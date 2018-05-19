;;; config-editing.el --- Config for simple editing packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-major-mode-hydra)



(cb-major-mode-hydra-define hexl-mode
  "Insert"
  (("d" hexl-insert-decimal-char "decimal")
   ("c" hexl-insert-octal-char "octal")
   ("x" hexl-insert-hex-char "hex")
   ("s" hexl-insert-hex-string "hex string"))

  "Address"
  (("g" hexl-goto-address "goto")))



(use-package hexl
  :defer t
  :config
  (evil-define-key 'motion hexl-mode-map
    "]]" 'hexl-end-of-1k-page
    "[[" 'hexl-beginning-of-1k-page
    "h" 'hexl-backward-char
    "l" 'hexl-forward-char
    "j" 'hexl-next-line
    "k" 'hexl-previous-line
    "$" 'hexl-end-of-line
    "^" 'hexl-beginning-of-line
    "0" 'hexl-beginning-of-line))

;; aggressive-indent automatically reindents code during editing.

(use-package aggressive-indent
  :straight t
  :commands (global-aggressive-indent-mode)
  :defer 1
  :init
  (defconst aggressive-indent-excluded-modes
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
  :defer 1

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
  :bind ("C-:" . tiny-expand))

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
  :defer 1
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
  :bind (:map
         evil-motion-state-map
         ("*" . evil-ahs/enter-ahs-forward)
         ("#" . evil-ahs/enter-ahs-backward)))

;; dump-jump provides a good fallback for navigating to definitions in the
;; absence of tags tables or semantic analysis.

(use-package dumb-jump
  :straight t
  :commands (dumb-jump-go dumb-jump-go-other-window)
  :preface
  (autoload 'pop-tag-mark "etags")
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal prog-mode-map (kbd "M-.") #'dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy))

(provide 'config-editing)

;;; config-editing.el ends here
