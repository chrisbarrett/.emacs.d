;;; cb-markdown.el --- Configuration for markdown-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'evil-define-key "evil")
(autoload 'evil-insert-state "evil")

(use-package which-key
  :config
  (push `((nil . ,(rx bos "markdown-" (? "insert-") (group (+ nonl)))) . (nil . "\\1"))
        which-key-replacement-alist))

(use-package markdown-mode
  :straight t
  :commands (markdown-mode
             gfm-mode
             markdown-cycle
             markdown-demote
             markdown-export
             markdown-follow-thing-at-point
             markdown-insert-blockquote
             markdown-insert-bold
             markdown-insert-footnote
             markdown-insert-gfm-code-block
             markdown-insert-header-dwim
             markdown-insert-hr
             markdown-insert-image
             markdown-insert-italic
             markdown-insert-kbd
             markdown-insert-link
             markdown-insert-strike-through
             markdown-insert-wiki-link
             markdown-live-preview-mode
             markdown-move-subtree-down
             markdown-move-subtree-up
             markdown-preview
             markdown-promote)

  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))

  :preface
  (progn

    (defun cb-markdown--evil-insert-state (&rest _)
      (evil-insert-state))

    (defun cb-markdown-electric-backquote (&optional arg)
      "Insert a backquote, possibly expanding to a source block."
      (interactive "*P")
      (self-insert-command (prefix-numeric-value arg))
      (when (and markdown-gfm-use-electric-backquote (looking-back (rx bol (>= 3 "`")) nil))
        (replace-match "")
        (when (looking-at (rx (+ "`")))
          (replace-match ""))
        (call-interactively #'markdown-insert-gfm-code-block))))

  :config
  (progn
    (setq markdown-command "multimarkdown")
    (setq markdown-fontify-code-blocks-natively t)
    (setq markdown-hide-urls t)

    (dolist (mode '(markdown-mode gfm-mode))
      (spacemacs-keys-declare-prefix-for-mode mode "m i" "insert")

      (spacemacs-keys-set-leader-keys-for-major-mode mode
        "if" #'markdown-insert-footnote
        "il" #'markdown-insert-link
        "ii" #'markdown-insert-image
        "ic" #'markdown-insert-gfm-code-block
        "ik" #'markdown-insert-kbd
        "i-" #'markdown-insert-hr
        "is" #'markdown-insert-strike-through
        "iw" #'markdown-insert-wiki-link

        "b" #'markdown-insert-bold
        "t" #'markdown-insert-italic
        "q" #'markdown-insert-blockquote

        "h" #'markdown-insert-header-dwim

        "m" #'markdown-live-preview-mode
        "p" #'markdown-preview
        "e" #'markdown-export))

    (advice-add 'markdown-insert-header-dwim :after #'cb-markdown--evil-insert-state)

    (evil-define-key 'insert markdown-mode-map (kbd "`") #'cb-markdown-electric-backquote)
    (evil-define-key 'normal markdown-mode-map (kbd "TAB") #'markdown-cycle)

    (define-key markdown-mode-map (kbd "C-c C-l") #'markdown-insert-link)
    (define-key markdown-mode-map (kbd "M-<left>") #'markdown-promote)
    (define-key markdown-mode-map (kbd "M-<right>") #'markdown-demote)
    (define-key markdown-mode-map (kbd "M-<up>") #'markdown-move-subtree-up)
    (define-key markdown-mode-map (kbd "M-<down>") #'markdown-move-subtree-down)
    (evil-define-key 'normal markdown-mode-map (kbd "RET") #'markdown-follow-thing-at-point)))

(use-package fence-edit
  :straight t
  :after markdown-mode
  :commands (fence-edit-code-at-point)
  :config
  (progn
    (add-to-list 'fence-edit-lang-modes '("lisp" . emacs-lisp-mode))
    (define-key markdown-mode-map (kbd "C-c '") #'fence-edit-code-at-point)))

(provide 'cb-markdown)

;;; cb-markdown.el ends here
