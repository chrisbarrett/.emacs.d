;;; cb-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'spacemacs-keys)
(require 'evil)
(require 'subr-x)

(define-derived-mode dir-locals-mode emacs-lisp-mode "dir-locals")
(add-to-list 'auto-mode-alist '("\\.dir-locals.el\\'" . dir-locals-mode))

(use-package lisp-mode
  :mode ("/Cask\\'" . lisp-mode))

(use-package elisp-mode
  :preface
  (defun cb-elisp/eval-buffer ()
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

  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'emacs-lisp-mode "m e" "eval")
    (spacemacs-keys-set-leader-keys-for-major-mode 'emacs-lisp-mode
      "eb" #'cb-elisp/eval-buffer
      "ee" #'eval-expression))

  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm))

(use-package ielm
  :commands ielm
  :preface
  (defun cb-elisp-pop-to-elisp-buffer ()
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
    (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-z") #'cb-elisp-pop-to-elisp-buffer)))

(use-package which-key
  :preface
  (autoload 'which-key-add-key-based-replacements "cb-leader-keys")
  :config
  (progn
    (which-key-add-key-based-replacements "SPC a p" "profiler")
    (push `(("SPC a p" . ,(rx bos "profiler-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)
    (push `((", e" . ,(rx bos "eval-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package elisp-slime-nav
  :straight t
  :commands (elisp-slime-nav-find-elisp-thing-at-point
             elisp-slime-nav-describe-elisp-thing-at-point)
  :bind
  (:map emacs-lisp-mode-map ("M-." . elisp-slime-nav-find-elisp-thing-at-point))
  :init
  (progn
    (evil-define-key 'normal elisp-slime-nav-mode-map
      (kbd "M-.") #'elisp-slime-nav-find-elisp-thing-at-point
      (kbd "K") #'elisp-slime-nav-describe-elisp-thing-at-point)

    (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

  :commands (turn-on-elisp-slime-nav-mode))

(use-package eldoc
  :commands (eldoc-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

(use-package nameless
  :straight t
  :commands nameless-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  :config
  (progn
    (setq nameless-prefix ":")
    (setq nameless-private-prefix t)))

(use-package cb-elisp-autoinsert
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
  :straight t
  :commands (package-lint-current-buffer))

(use-package flycheck-package
  :straight t
  :after flycheck
  :preface
  (autoload 'flycheck-package-setup "flycheck-package")
  :config (flycheck-package-setup))

(use-package profiler
  :commands (profiler-start profiler-report profiler-stop)
  :init
  (spacemacs-keys-set-leader-keys
    "app" 'profiler-start
    "apr" 'profiler-report
    "aps" 'profiler-stop)

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

(use-package emr
  :disabled t
  :commands (emr-show-refactor-menu)
  :ensure t
  :init
  (define-key prog-mode-map (kbd "C-<return>") #'emr-show-refactor-menu)
  :config
  (emr-initialize))

(provide 'cb-elisp)

;;; cb-elisp.el ends here
