;;; config-markdown.el --- Configuration for markdown-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)
(autoload 'evil-define-key "evil")
(autoload 'evil-insert-state "evil")



(dolist (mode '(gfm-mode markdown-mode))
  (eval
   `(progn
      (major-mode-hydra-bind ,mode "Insert"
        ("ih" markdown-insert-header-dwim "header")
        ("ic" markdown-insert-gfm-code-block "code block")
        ("ii" markdown-insert-image "image")
        ("if" markdown-insert-footnote "footnote")
        ("il" markdown-insert-link "link")
        ("iw" markdown-insert-wiki-link "wiki link")
        ("i-" markdown-insert-hr "horizontal rule"))

      (major-mode-hydra-bind ,mode "Markup"
        ("mb" markdown-insert-bold)
        ("mi" markdown-insert-italic)
        ("mk" markdown-insert-kbd "keyboard code")
        ("mq" markdown-insert-blockquote)
        ("ms" markdown-insert-strike-through "strikethrough"))

      (major-mode-hydra-bind ,mode "Actions"
        ("o" markdown-preview "open in browser")
        ("p" markdown-live-preview-mode "live preview mode")
        ("e" markdown-export)))))



(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :preface
  (progn

    (defun config-markdown--evil-insert-state (&rest _)
      (evil-insert-state))

    (defun config-markdown-electric-backquote (&optional arg)
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

    (advice-add 'markdown-insert-header-dwim :after #'config-markdown--evil-insert-state)

    (evil-define-key 'insert markdown-mode-map (kbd "`") #'config-markdown-electric-backquote)
    (evil-define-key 'normal markdown-mode-map (kbd "TAB") #'markdown-cycle)

    (define-key markdown-mode-map (kbd "C-c C-l") #'markdown-insert-link)
    (define-key markdown-mode-map (kbd "M-<left>") #'markdown-promote)
    (define-key markdown-mode-map (kbd "M-<right>") #'markdown-demote)
    (define-key markdown-mode-map (kbd "M-<up>") #'markdown-move-subtree-up)
    (define-key markdown-mode-map (kbd "M-<down>") #'markdown-move-subtree-down)

    (evil-define-key 'normal markdown-mode-map (kbd "RET") #'markdown-follow-thing-at-point)))

(use-package edit-indirect
  :straight t
  :after markdown-mode)

(provide 'config-markdown)

;;; config-markdown.el ends here
