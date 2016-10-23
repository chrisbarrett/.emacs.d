;;; cb-yasnippet.el --- Configure yasnippet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)
(require 's)


(use-package yasnippet
  :commands (yas-global-mode yas-expand yas-new-snippet yas-insert-snippet)

  :preface
  (progn
    (autoload 'yas--field-contains-point-p "yasnippet")
    (autoload 'yas--field-text-for-display "yasnippet")

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

    (defun cb-yasnippet--maybe-goto-field-end ()
      "Move to the end of the current field if it has been modified."
      (when-let (field (cb-yasnippet--current-field))
        (when (and (yas--field-modified-p field)
                   (yas--field-contains-point-p field))
          (goto-char (cb-yasnippet--end-of-field)))))

    (defun cb-yasnippet-goto-field-end (&rest _)
      (cb-yasnippet--maybe-goto-field-end)
      (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-insert-state))
        (evil-insert-state)))

    (defun cb-yasnippet--current-field ()
      "Return the current active field."
      (and (boundp 'yas--active-field-overlay)
           yas--active-field-overlay
           (overlay-buffer yas--active-field-overlay)
           (overlay-get yas--active-field-overlay 'yas--field)))

    (defun cb-yasnippet--start-of-field ()
      (when-let (field (cb-yasnippet--current-field))
        (marker-position (yas--field-start field))))

    (defun cb-yasnippet--end-of-field ()
      (when-let (field (cb-yasnippet--current-field))
        (marker-position (yas--field-end field))))

    (defun cb-yasnippet--current-field-text ()
      "Return the text in the active snippet field."
      (when-let (field (cb-yasnippet--current-field))
        (yas--field-text-for-display field)))

    (defun cb-yasnippet-clear-blank-field (&rest _)
      "Clear the current field if it is blank."
      (when-let ((beg (cb-yasnippet--start-of-field))
                 (end (cb-yasnippet--end-of-field))
                 (str (cb-yasnippet--current-field-text)))
        (when (s-matches? (rx bos (+ space) eos) str)
          (delete-region beg end)
          t))))

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

    (advice-add #'yas-next-field :before #'cb-yasnippet-clear-blank-field)
    (advice-add #'yas-prev-field :before #'cb-yasnippet-clear-blank-field)
    (advice-add #'yas-next-field :after #'cb-yasnippet-goto-field-end)
    (advice-add #'yas-prev-field :after #'cb-yasnippet-goto-field-end)

    ;; Ensure yasnippet expansion preserves current indentation. This can be a
    ;; problem in modes with significant whitespace, where the indentation
    ;; command unconditionally indents one step.

    (advice-add 'yas--expand-or-prompt-for-template :around #'cb-yasnippet-preserve-indentation))

  :commands
  (yas-visit-snippet-file
   yas-expand
   yas-new-snippet
   yas-insert-snippet
   yas-next-field
   yas-prev-field))

(provide 'cb-yasnippet)

;;; cb-yasnippet.el ends here
