;;; cb-lang-markdown.el --- Configuration for Markdown dialects  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)

(use-package markdown-mode
  :general
  (:states 'normal :keymaps 'markdown-mode-map
   "TAB" #'markdown-cycle
   "RET" #'markdown-follow-thing-at-point)
  (:keymaps 'markdown-mode-map
   "C-c C-l" #'markdown-insert-link
   "C-c C-i" #'markdown-insert-image
   "C-c C-f" #'markdown-insert-footnote
   "C-c C--" #'markdown-insert-hr
   "C-c C-e" #'markdown-export
   "C-c C-o" #'markdown-preview
   "C-c p" #'markdown-live-preview-mode
   "C-<return>" #'markdown-insert-header-dwim
   "M-<left>" #'markdown-promote
   "M-<right>" #'markdown-demote
   "M-<up>" #'markdown-move-subtree-up
   "M-<down>" #'markdown-move-subtree-down)

  :custom
  (markdown-asymmetric-header t)
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)
  (markdown-hr-display-char ?\^L))

(mode-leader-set-key :keymaps '(gfm-mode-map markdown-mode-map)
  "i" '(nil :wk "insert")
  "i h" '(markdown-insert-header-dwim :wk "header")
  "i c" '(markdown-insert-gfm-code-block :wk "code block...")
  "i i" '(markdown-insert-image :wk "image")
  "i f" '(markdown-insert-footnote :wk "footnote")
  "i l" '(markdown-insert-link :wk "link")
  "i w" '(markdown-insert-wiki-link :wk "wiki link")
  "i -" '(markdown-insert-hr :wk "hr")

  "m" '(nil :wk "markup")
  "m b" '(markdown-insert-bold :wk "bold")
  "m i" '(markdown-insert-italic :wk "italic")
  "m k" '(markdown-insert-kbd :wk "kbd")
  "m q" '(markdown-insert-blockquote :wk "blockquote")
  "m s" '(markdown-insert-strike-through :wk "strike-through")

  "o" '(markdown-preview :wk "preview")
  "p" '(markdown-live-preview-mode :wk "preview (live)")
  "e" '(markdown-export :wk "export"))

(provide 'cb-lang-markdown)

;;; cb-lang-markdown.el ends here
