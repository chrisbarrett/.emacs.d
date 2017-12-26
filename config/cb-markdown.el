;;; cb-markdown.el --- Configuration for markdown-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'evil-insert-state "evil-states")

(with-eval-after-load 'which-key
  (with-no-warnings
    (push `((nil . ,(rx bos "markdown-" (? "insert-") (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package markdown-mode
  :commands (markdown-mode
             gfm-mode
             markdown-insert-footnote
             markdown-insert-link
             markdown-insert-image
             markdown-insert-gfm-code-block
             markdown-insert-hr
             markdown-insert-bold
             markdown-insert-italic
             markdown-insert-strike-through
             markdown-insert-header-dwim
             markdown-insert-header-setext-dwim
             markdown-promote
             markdown-demote
             markdown-move-subtree-up
             markdown-move-subtree-down)

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :preface
  (defun cb-markdown--evil-insert-state (&rest _)
    (evil-insert-state))

  :config
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'markdown-mode "m i" "insert")

    (spacemacs-keys-set-leader-keys-for-major-mode 'markdown-mode
      "if" #'markdown-insert-footnote
      "il" #'markdown-insert-link
      "ii" #'markdown-insert-image
      "ic" #'markdown-insert-gfm-code-block
      "i-" #'markdown-insert-hr

      "b" #'markdown-insert-bold
      "t" #'markdown-insert-italic
      "s" #'markdown-insert-strike-through

      "h" #'markdown-insert-header-dwim
      "H" #'markdown-insert-header-setext-dwim)

    (advice-add 'markdown-insert-header-dwim :after #'cb-markdown--evil-insert-state)

    (define-key markdown-mode-map (kbd "C-c C-l") #'markdown-insert-link)
    (define-key markdown-mode-map (kbd "M-<left>") #'markdown-promote)
    (define-key markdown-mode-map (kbd "M-<right>") #'markdown-demote)
    (define-key markdown-mode-map (kbd "M-<up>") #'markdown-move-subtree-up)
    (define-key markdown-mode-map (kbd "M-<down>") #'markdown-move-subtree-down)))

(use-package fence-edit
  :after markdown-mode
  :commands (fence-edit-code-at-point)
  :config
  (progn
    (add-to-list 'fence-edit-lang-modes '("lisp" . emacs-lisp-mode))
    (define-key markdown-mode-map (kbd "C-c '") #'fence-edit-code-at-point)))

(provide 'cb-markdown)

;;; cb-markdown.el ends here
