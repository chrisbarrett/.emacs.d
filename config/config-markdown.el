;;; config-markdown.el --- Configuration for markdown-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)

(autoload 'evil-insert-state "evil")

(dolist (mode '(gfm-mode markdown-mode))
  (eval
   `(major-mode-hydra-define ,mode nil
      ("Insert"
       (("ih" markdown-insert-header-dwim "header")
        ("ic" markdown-insert-gfm-code-block "code block")
        ("ii" markdown-insert-image "image")
        ("if" markdown-insert-footnote "footnote")
        ("il" markdown-insert-link "link")
        ("iw" markdown-insert-wiki-link "wiki link")
        ("i-" markdown-insert-hr "horizontal rule"))
       "Markup"
       (("mb" markdown-insert-bold)
        ("mi" markdown-insert-italic)
        ("mk" markdown-insert-kbd "keyboard code")
        ("mq" markdown-insert-blockquote)
        ("ms" markdown-insert-strike-through "strikethrough"))
       "Actions"
       (("o" markdown-preview "open in browser")
        ("p" markdown-live-preview-mode "live preview mode")
        ("e" markdown-export))))))



(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))

  :general (:states 'insert :keymaps 'markdown-mode-map "`" #'config-markdown-electric-backquote)
  :general (:states 'normal :keymaps 'markdown-mode-map
            "TAB" #'markdown-cycle
            "RET" #'markdown-follow-thing-at-point)
  :general (:keymaps 'markdown-mode-map
            "C-c C-l" #'markdown-insert-link
            "M-<left>" #'markdown-promote
            "M-<right>" #'markdown-demote
            "M-<up>" #'markdown-move-subtree-up
            "M-<down>" #'markdown-move-subtree-down)
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
    (advice-add 'markdown-insert-header-dwim :after #'config-markdown--evil-insert-state)))

(use-package edit-indirect
  :defer t
  :general ("C-c r" 'edit-indirect-region)
  :after markdown-mode)

(provide 'config-markdown)

;;; config-markdown.el ends here
