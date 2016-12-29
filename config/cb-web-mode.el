;;; cb-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package web-mode
  :defines (web-mode-markup-indent-offset
            web-mode-css-indent-offset)

  :preface
  (autoload 'sp-local-pair "smartparens")

  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)

    ;; Disable web-mode-reload binding
    (define-key web-mode-map (kbd "C-c C-r") nil)

    ;; Treat es6 files as JS files.

    (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
    (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))))

(use-package cb-web-modes
  :defer t
  :mode (("\\.json\\'" . cb-web-json-mode)
         ("\\.es6\\'"  . cb-web-js-mode)
         ("\\.jsx?\\'" . cb-web-js-mode)
         ("\\.html\\'" . cb-web-html-mode))
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'json-jsonlint 'cb-web-json-mode)))


(use-package flycheck
  :defer t
  :functions (flycheck-add-mode)
  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
    (flycheck-add-mode 'javascript-eslint 'cb-web-js-mode)))


(use-package emmet-mode
  :defer t
  :defines (emmet-expand-jsx-className?)
  :commands (emmet-mode)
  :preface
  (defun cb-web--set-jsx-classname-on ()
    (setq-local emmet-expand-jsx-className? t))
  :init
  (add-hook 'web-mode-hook #'emmet-mode)
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (define-key emmet-mode-keymap (kbd "TAB") #'emmet-expand-line)
    (add-hook 'cb-web-js-mode-hook #'cb-web--set-jsx-classname-on)))

(use-package cb-flow-checker
  :defer t
  :after flycheck
  :config
  (progn
    (add-hook 'cb-web-js-mode-hook (lambda ()
                               (flycheck-select-checker 'javascript-flow)))
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))

(use-package cb-flow
  :after cb-web-modes
  :init
  (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-js-mode
    "if" #'cb-flow-insert-flow-annotation)
  :bind (:map cb-web-js-mode-map
              ("C-c C-t" . cb-flow-type-at)))

(provide 'cb-web-mode)

;;; cb-web-mode.el ends here
