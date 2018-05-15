;;; config-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'evil)
(require 'cb-major-mode-hydra)
(require 'straight)
(require 'subr-x)

(autoload 'ert-select-tests "ert")



(cb-major-mode-hydra-define emacs-lisp-mode
  "Eval"
  (("eb" config-elisp-eval-buffer "buffer")
   ("ee" eval-expression "expression"))

  "Debug"
  (("d" debug-on-entry "on function called")
   ("cd" cancel-debug-on-entry "cancel debug function")
   ("v" debug-on-variable-change "on variable changed")
   ("cv" cancel-debug-on-variable-change "cancel debug variable"))

  "Test"
  (("tt" (ert t) "run all")
   ("tf" (ert-select-tests :failed t) "run failing")
   ("tn" (ert-select-tests :passed t) "run new")
   ("ts" ert "run with selector")))


;; Define some modes for specific files.

(define-derived-mode dir-locals-mode emacs-lisp-mode "dir-locals")
(add-to-list 'auto-mode-alist '("\\.dir-locals.el\\'" . dir-locals-mode))

(define-derived-mode caskfile-mode emacs-lisp-mode "caskfile")
(add-to-list 'auto-mode-alist '("/Cask\\'" . caskfile-mode))


;; Define an eval-buffer command for elisp that executes within a straight
;; transaction.

(defun config-elisp-eval-buffer ()
  "Evaluate the current buffer as Elisp code, within a straight transaction."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (null buffer-file-name)
        (eval-buffer)
      (when (string= buffer-file-name user-init-file)
        (straight-mark-transaction-as-init))
      (load buffer-file-name nil 'nomessage)))
  (message "Evaluating %s... done." (buffer-name)))


;; IELM is the Elisp repl built in to Emacs.

(use-package ielm
  :bind (:map emacs-lisp-mode-map ("C-c C-z" . ielm))
  :preface
  (defun config-elisp-pop-to-elisp-buffer ()
    (interactive)
    (if-let* ((buf (seq-find (lambda (buf)
                               (with-current-buffer buf
                                 (derived-mode-p 'emacs-lisp-mode)))
                             (buffer-list))))
        (pop-to-buffer buf)
      (user-error "No Emacs Lisp buffers")))
  :config
  (progn
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*ielm*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))
    (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-z") #'config-elisp-pop-to-elisp-buffer)))

;; elisp-slime-nav provides a command for navigating to the definitions of
;; things in elisp in a uniform way.

(use-package elisp-slime-nav
  :straight t
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  :bind
  (:map emacs-lisp-mode-map ("M-." . elisp-slime-nav-find-elisp-thing-at-point))
  :init
  (evil-define-key 'normal elisp-slime-nav-mode-map
    (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point
    (kbd "K") #'elisp-slime-nav-describe-elisp-thing-at-point))

;; eldoc shows function parameters in the minibuffer.

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

;; nameless hides the package prefix in elisp buffers, which can make things
;; easier to read.

(use-package nameless
  :straight t
  :commands nameless-mode
  :hook (emacs-lisp-mode . nameless-mode)
  :config
  (progn
    (setq nameless-prefix ":")
    (setq nameless-private-prefix t)))

;; ert is the built-in elisp test runner.

(use-package ert
  :commands (ert)
  :config (evil-set-initial-state 'ert-simple-view-mode 'motion))

(provide 'config-elisp)

;;; config-elisp.el ends here
