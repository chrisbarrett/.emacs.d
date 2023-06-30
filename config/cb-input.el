;;; cb-input.el --- Configure input methods etc.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Configure Emacs so that each paragraph may have a different text direction.
(setq-default bidi-paragraph-separate-re "^")
(setq-default bidi-paragraph-start-re "^")

;; Instantly display current keystrokes in minibuffer
(setq echo-keystrokes 0.02)

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

(provide 'cb-input)

;;; cb-input.el ends here
