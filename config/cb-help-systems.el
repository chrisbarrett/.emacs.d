;;; cb-help-systems.el --- Configuration for info, help, etc.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)

(use-package info
  :general
  (:states 'normal :keymaps 'Info-mode-map
   "^" 'Info-up
   "<" 'Info-history-back
   ">" 'Info-history-forward
   "<left>" 'Info-prev
   "<right>" 'Info-next
   "[" 'Info-prev
   "]" 'Info-next
   "L" 'Info-forward-node
   "C-n" 'Info-forward-node
   "H" 'Info-backward-node
   "C-p" 'Info-backward-node))

(use-package info+
  :after info
  :disabled t ; throws error on load
  :demand t
  :custom
  (Info-fontify-angle-bracketed-flag nil))

(use-package info-path-from-nix
  :after info
  :autoload (info-path-from-nix)
  :demand t
  :config
  (setq Info-directory-list (append Info-default-directory-list
                                    (info-path-from-nix))))

(use-package help-mode
  :general
  (:states 'motion :keymaps 'help-mode-map
   "<escape>" 'quit-window
   "^" 'help-go-back
   "gh" 'help-follow-symbol))

;; TODO: what is the difference between `man' and `woman'? Which should I
;; generally prefer?

(use-package man
  :general (:keymaps 'Man-mode-map
            "M-n" #'Man-next-section
            "M-p" #'Man-previous-section))

;; Use MANPATH to look up completion candidates.
(use-package man-completing
  :after 'man
  :autoload (man-completing-mode)
  :demand t
  :config
  (man-completing-mode +1))

(use-package apropos
  :custom
  (apropos-do-all t))

(use-package woman
  :custom
  (woman-fill-frame t)
  (woman-default-indent 7))

(use-package helpful
  :preface
  (defun cb-use-helpful-for-evil-lookup ()
    (setq-local evil-lookup-func 'helpful-at-point))
  :hook ((emacs-lisp-mode . cb-use-helpful-for-evil-lookup)
         (helpful-mode . cb-use-helpful-for-evil-lookup)
         (ielm-mode . cb-use-helpful-for-evil-lookup)))

(provide 'cb-help-systems)

;;; cb-help-systems.el ends here
