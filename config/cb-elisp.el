;;; cb-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'evil)

(use-package lisp-mode
  :mode ("/Cask\\'" . lisp-mode))

(use-package elisp-mode
  :preface
  (defun cb-elisp--message-on-eval-buffer (&rest _)
    (when (called-interactively-p nil)
      (message "Buffer evaluated.")))

  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'emacs-lisp-mode "m e" "Eval")

    (spacemacs-keys-set-leader-keys-for-major-mode 'emacs-lisp-mode
      "eb" #'eval-buffer
      "ee" #'eval-expression))

  :config
  (advice-add #'eval-buffer :after #'cb-elisp--message-on-eval-buffer))

(use-package elisp-slime-nav
  :commands (elisp-slime-nav-find-elisp-thing-at-point
             elisp-slime-nav-describe-elisp-thing-at-point)
  :bind
  (:map emacs-lisp-mode-map ("M-." . elisp-slime-nav-find-elisp-thing-at-point))
  :init
  (progn
    (evil-define-key 'normal emacs-lisp-mode-map
      (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point
      (kbd "K") #'elisp-slime-nav-describe-elisp-thing-at-point)

    (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

  :commands (turn-on-elisp-slime-nav-mode))

(use-package eldoc
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

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

(use-package cb-elisp-autoinsert
  :defer t
  :after autoinsert
  :config
  (add-to-list 'auto-insert-alist cb-elisp-autoinsert-form)
  :defines (auto-insert-alist))

;; Checkdoc configuration

(use-package cb-flycheck-checkdoc
  :after flycheck
  :config
  (setq flycheck-emacs-lisp-checkdoc-form cb-flycheck-checkdoc-form))

(use-package checkdoc
  :defer t
  :config
  (progn
    (setq checkdoc-force-docstrings-flag nil)
    (setq checkdoc-arguments-in-order-flag nil)))

(use-package ert
  :commands (ert)
  :preface
  (defun cb/ert-run-all-tests ()
    (interactive)
    (ert t))
  :init
  (spacemacs-keys-set-leader-keys-for-major-mode 'emacs-lisp-mode
    "t" #'cb/ert-run-all-tests)
  :config
  (evil-set-initial-state 'ert-simple-view-mode 'motion))

(use-package package-lint
  :commands (package-lint-current-buffer))

(use-package flycheck-package
  :after flycheck
  :preface
  (autoload 'flycheck-package-setup "flycheck-package")
  :config (flycheck-package-setup))

(use-package profiler
  :commands (profiler-start profiler-report profiler-stop)
  :init
  (progn
    (use-package which-key
      :preface (autoload 'which-key-add-key-based-replacements "which-key")
      :config (which-key-add-key-based-replacements "SPC a p" "profiler"))

    (spacemacs-keys-set-leader-keys
      "app" 'profiler-start
      "apr" 'profiler-report
      "aps" 'profiler-stop))

  :config
  (progn
    (evil-set-initial-state 'profiler-report-mode 'motion)
    (evil-define-key 'motion profiler-report-mode-map
      "j" 'profiler-report-next-entry
      "k" 'profiler-report-previous-entry
      "n" 'profiler-report-next-entry
      "p" 'profiler-report-previous-entry

      (kbd "TAB") 'profiler-report-toggle-entry
      (kbd "K") 'profiler-report-describe-entry
      (kbd "RET") 'profiler-report-find-entry
      (kbd "=") 'profiler-report-compare-profile

      "g r" 'revert-buffer
      "B" 'profiler-report-render-reversed-calltree
      "f" 'profiler-report-find-entry)))

(provide 'cb-elisp)

;;; cb-elisp.el ends here
