;;; cb-smartparens.el --- Smartparens config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <admiral@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package smartparens
  :defer t
  :config

  :preface
  (progn
    (defun cb-smartparens--sp-for-eval-expression ()
      (when (eq this-command 'eval-expression)
        (smartparens-mode)))

    (defun cb-smartparens-newline-post-handler (_id _action _context)
      (save-excursion
        (newline)
        (indent-according-to-mode))
      (indent-according-to-mode)))

  :init
  (progn
    (add-hook 'prog-mode-hook #'smartparens-strict-mode)
    (add-hook 'text-mode-hook #'smartparens-strict-mode)
    (add-hook 'text-mode-hook #'smartparens-strict-mode)
    (add-hook 'minibuffer-setup-hook #'cb-smartparens--sp-for-eval-expression))

  :config
  (progn
    (setq sp-show-pair-delay 0.2)
    (setq sp-show-pair-from-inside t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-highlight-pair-overlay nil)
    (setq sp-highlight-wrap-overlay nil)
    (setq sp-highlight-wrap-tag-overlay nil)
    (setq sp-navigate-close-if-unbalanced t)
    (setq sp-message-width nil)

    (require 'smartparens-config)

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    (sp-pair "(" ")"   :bind "M-(")
    (sp-pair "{" "}"   :bind "M-{")
    (sp-pair "[" "]"   :bind "M-[")
    (sp-pair "\"" "\"" :bind "M-\"")
    (sp-pair "`" "`"   :bind "M-`")

    (sp-pair "{" nil :post-handlers
             '(:add (cb-smartparens-newline-post-handler "RET")))
    (sp-pair "[" nil :post-handlers
             '(:add (cb-smartparens-newline-post-handler "RET")))

    (smartparens-global-strict-mode +1)
    (show-smartparens-global-mode +1))

  :functions (sp-local-pair sp-pair)
  :commands (smartparens-mode
             smartparens-strict-mode
             smartparens-global-strict-mode
             show-smartparens-global-mode))


(provide 'cb-smartparens)

;;; cb-smartparens.el ends here
