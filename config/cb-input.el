;;; cb-input.el --- Configure input methods etc.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Configure Emacs so that each paragraph may have a different text direction.
(setq-default bidi-paragraph-separate-re "^")
(setq-default bidi-paragraph-start-re "^")

;; Enable useful commands that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)




(use-package paragraphs
  :custom
  ;; Don't require two spaces to signal the end of a sentence. I don't use
  ;; sentence-based commands that often anyway.
  (sentence-end-double-space nil))

(use-package mule
  :custom
  (default-input-method "TeX")
  :preface
  ;; Set up LaTeX-style input method and add extra MULE rules for common chars.
  (defun cb--set-tex-input-method-vars ()
    (when-let* ((quail-current-package (assoc "TeX" quail-package-alist)))
      (quail-defrule ";" (quail-lookup-key "\\"))
      ;; NOTE: Use hex literals instead of unicode chars so Nix doesn't choke
      ;; parsing this form.
      (quail-define-rules ((append . t))
                          ("\\ou" #x16f) ; ů
                          ("\\null" #x2205) ; ∅
                          ("\\rarr" #x2192) ; →
                          ("\\larr" #x2190) ; ←
                          ("\\lr" #x2194) ; ↔
                          ("\\lam" #x3bb) ; λ
                          ("\\Lam" #x39b) ; Λ
                          ("\\all" #x2200) ; ∀
                          ("\\rtack" #x22a2) ; ⊢
                          )))
  :config
  (add-hook 'input-method-activate-hook #'cb--set-tex-input-method-vars))

(use-package mule-cmds
  :config
  ;; Use UTF-8 everywhere
  ;; See:
  ;; https://www.reddit.com/r/emacs/comments/siuvpu/comment/hvbns5f
  (set-language-environment "UTF-8")
  (set-locale-environment "en_NZ.UTF-8"))

(use-package comint
  :custom
  (comint-prompt-read-only t))



;;; Minibuffer

;; Instantly display current keystrokes in minibuffer
(setq echo-keystrokes 0.02)

;; Limit the max height of the minibuffer
(setq max-mini-window-height 0.1)

;; Prevent cursor from entering the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)



(use-package password-cache
  :custom
  (password-cache t)
  (password-cache-expiry 300))

(use-package better-eval-expression
  :general
  ([remap eval-expression] 'better-eval-expression)

  :preface
  (defun cb-better-eval-expr-in-debugger (f exp)
    (interactive (list (read (better-eval-expression--read "Eval (in frame): "))))
    (funcall f exp))
  (advice-add 'debugger-eval-expression :around #'cb-better-eval-expr-in-debugger)
  (advice-add 'debugger-record-expression :around #'cb-better-eval-expr-in-debugger))

(provide 'cb-input)

;;; cb-input.el ends here
