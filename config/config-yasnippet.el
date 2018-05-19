;;; config-yasnippet.el --- Configure yasnippet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'subr-x)
(require 's)
(require 'warnings)

;; Sometimes yasnippet causes this warning to be omitted. Since I don't control
;; the code, it's not useful.
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

(use-package yasnippet
  :straight t
  :defer 1

  :preface
  (progn
    (autoload 'sp-backward-delete-char "smartparens")
    (autoload 'evil-define-key "evil")

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
      (when-let* ((field (cb-yasnippet--current-field)))
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
      (when-let* ((field (cb-yasnippet--current-field)))
        (marker-position (yas--field-start field))))

    (defun cb-yasnippet--end-of-field ()
      (when-let* ((field (cb-yasnippet--current-field)))
        (marker-position (yas--field-end field))))

    (defun cb-yasnippet--current-field-text ()
      "Return the text in the active snippet field."
      (when-let* ((field (cb-yasnippet--current-field)))
        (yas--field-text-for-display field)))

    (defun cb-yasnippet-clear-blank-field (&rest _)
      "Clear the current field if it is blank."
      (when-let* ((beg (cb-yasnippet--start-of-field))
                  (end (cb-yasnippet--end-of-field))
                  (str (cb-yasnippet--current-field-text)))
        (when (s-matches? (rx bos (+ space) eos) str)
          (delete-region beg end)
          t)))


    (defun cb-yasnippet-space ()
      "Clear and skip this field if it is unmodified.  Otherwise insert a space."
      (interactive "*")
      (let ((field (cb-yasnippet--current-field))
            ;; (sp-mode? (and (boundp 'smartparens-mode) smartparens-mode))
            )
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              ;; (sp-mode?
              ;;  (sp-generic-prog-space))
              (t
               (call-interactively #'self-insert-command)))))

    (defun cb-yasnippet-backspace ()
      "Clear the current field if the current snippet is unmodified.
Otherwise delete backwards."
      (interactive "*")
      (let ((field (cb-yasnippet--current-field))
            (sp-mode? (and (boundp 'smartparens-mode) smartparens-mode)))
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              ;; ((and sp-mode? (derived-mode-p 'prog-mode))
              ;;  (sp-generic-prog-backspace))
              (sp-mode?
               (call-interactively #'sp-backward-delete-char))
              (t
               (call-interactively #'backward-delete-char))))))

  :init
  ;; Fix malformed face decl
  (defface yas-field-highlight-face
    '((t (:inherit region)))
    "The face used to highlight the currently active field of a snippet")

  :config
  (progn
    (setq yas-wrap-around-region t)
    (setq yas-prompt-functions '(yas-completing-prompt))
    (setq yas-verbosity 0)
    (setq yas-minor-mode-map (make-sparse-keymap))

    (yas-global-mode +1)

    (add-to-list 'yas-dont-activate-functions (lambda () (derived-mode-p 'term-mode)))

    ;; Define key bindings for fancy snippet navigation.

    (bind-key (kbd "TAB") #'yas-expand yas-minor-mode-map)
    (evil-define-key 'insert yas-minor-mode-map (kbd "TAB") #'yas-expand)

    (evil-define-key 'insert yas-keymap (kbd "SPC") #'cb-yasnippet-space)
    (bind-key (kbd "<backspace>") #'cb-yasnippet-backspace yas-keymap)

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
  (yas-expand
   yas-global-mode
   yas-insert-snippet
   yas-new-snippet
   yas-next-field
   yas-prev-field
   yas-visit-snippet-file)

  :functions
  (yas--skip-and-clear
   yas--field-contains-point-p
   yas--field-text-for-display))

(use-package yas-funcs
  :after yasnippet)

(provide 'config-yasnippet)

;;; config-yasnippet.el ends here
