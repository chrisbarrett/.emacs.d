;;; cb-yasnippet.el --- Configure yasnippet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package yasnippet
  :commands (yas-global-mode yas-expand yas-new-snippet yas-insert-snippet)

  :preface
  (progn
    (defun cb-yasnippet-preserve-indentation (f &rest args)
      (let ((col
             (save-excursion
               (back-to-indentation)
               (current-column))))
        (apply f args)
        (save-excursion
          (atomic-change-group
            (goto-char (line-beginning-position))
            (delete-horizontal-space)
            (indent-to col)))))

    (defun cb-yasnippet-goto-field-end (&rest _)
      (yas/maybe-goto-field-end)
      (when (and (boundp 'evil-mode) evil-mode)
        (evil-insert-state))))

  :init
  (progn
    (spacemacs-keys-declare-prefix "y" "yasnippet")
    (spacemacs-keys-set-leader-keys
      "yf" #'yas-visit-snippet-file
      "ye" #'yas-expand
      "yn" #'yas-new-snippet
      "yy" #'yas-insert-snippet))

  :config
  (progn
    (setq yas-wrap-around-region t)
    (setq yas-prompt-functions '(yas-completing-prompt))
    (setq yas-verbosity 0)
    (setq yas-minor-mode-map (make-sparse-keymap))
    (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))

    (yas-global-mode +1)

    ;; Advise editing commands.
    ;;
    ;; Pressing SPC in an unmodified field will clear it and switch to the next.
    ;;
    ;; Pressing S-TAB to go to last field will place point at the end of the field.

    (advice-add 'yas-next-field :before #'yas/clear-blank-field)
    (advice-add 'yas-prev-field :before #'yas/clear-blank-field)
    (advice-add 'yas-next-field :after #'cb-yasnippet-goto-field-end)
    (advice-add 'yas-prev-field :after #'cb-yasnippet-goto-field-end)

    ;; Ensure yasnippet expansion preserves current indentation. This can be a
    ;; problem in modes with significant whitespace, where the indentation
    ;; command unconditionally indents one step.

    (advice-add 'yas--expand-or-prompt-for-template :around #'cb-yasnippet-preserve-indentation)))

(provide 'cb-yasnippet)

;;; cb-yasnippet.el ends here
