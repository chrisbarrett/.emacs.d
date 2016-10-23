;;; cb-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(require 'spacemacs-keys)

;; Print a message on `eval-buffer'.

(defun cb-elisp--message-on-eval-buffer (&rest _)
  (when (called-interactively-p nil)
    (message "Buffer evaluated.")))

(advice-add #'eval-buffer :after #'cb-elisp--message-on-eval-buffer)

(spacemacs-keys-set-leader-keys-for-major-mode 'emacs-lisp-mode
  "C-c C-f" #'eval-buffer)


(use-package find-func
  :leader-bind
  (("hfl" . find-library)
   ("hff" . find-function)
   ("hfv" . find-variable)
   ("hfc" . find-face-definition)))

(use-package elisp-slime-nav
  :bind
  (:map emacs-lisp-mode-map
        ("M-." . elisp-slime-nav-find-elisp-thing-at-point))

  :evil-bind
  (:map emacs-lisp-mode-map
        :state normal
        ("M-." . elisp-slime-nav-find-elisp-thing-at-point)
        ("K" . elisp-slime-nav-describe-elisp-thing-at-point))

  :init
  (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)

  :commands (turn-on-elisp-slime-nav-mode))

(use-package eldoc
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package nameless
  :commands nameless-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  :config
  (progn
    (setq nameless-prefix ":")
    (setq nameless-private-prefix t)))

(use-package evil-surround
  :defer t
  :preface
  (defun cb-elisp--init-evil-surround-pairs ()
    (make-local-variable 'evil-surround-pairs-alist)
    (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))
  :config
  (add-hook 'emacs-lisp-mode-hook #'cb-elisp--init-evil-surround-pairs))

(provide 'cb-elisp)

;;; cb-elisp.el ends here
