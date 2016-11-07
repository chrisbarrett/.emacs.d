;;; cb-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.es6\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode))

  :defines (web-mode-markup-indent-offset
            web-mode-css-indent-offset)

  :preface
  (autoload 'sp-local-pair "smartparens")

  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)

    ;; Treat es6 files as JS files.

    (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))

    ;; Smartparens+web-mode integration.

    (defun cb-web--sp-web-mode-is-code-context (id action context)
      (and (eq action 'insert)
           (not (or (get-text-property (point) 'part-side)
                    (get-text-property (point) 'block-side)))))

    (with-eval-after-load 'smartparens
      (sp-local-pair 'web-mode "<" nil :when '(cb-web--sp-web-mode-is-code-context)))))


(provide 'cb-web-mode)

;;; cb-web-mode.el ends here
