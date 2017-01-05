;;; cb-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'evil-define-key "evil-core")

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
         ("\\.css\\'"  . cb-web-css-mode)
         ("\\.html\\'" . cb-web-html-mode))
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'cb-web-js-mode)
    (flycheck-add-mode 'javascript-gjslint 'cb-web-js-mode)
    (flycheck-add-mode 'javascript-jscs 'cb-web-js-mode)
    (flycheck-add-mode 'javascript-jshint 'cb-web-js-mode)
    (flycheck-add-mode 'css-csslint 'cb-web-css-mode)
    (flycheck-add-mode 'json-jsonlint 'cb-web-json-mode)
    (flycheck-add-mode 'html-tidy 'cb-web-html-mode)))

(use-package flycheck
  :commands (flycheck-select-checker)
  :functions (flycheck-add-next-checker flycheck-add-mode)
  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
    (flycheck-add-mode 'javascript-eslint 'cb-web-js-mode)))


(use-package emmet-mode
  :defer t
  :defines (emmet-expand-jsx-className?)
  :commands (emmet-mode emmet-expand-line)
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
  :commands (cb-flow-insert-flow-annotation
             cb-flow-type-at)
  :init
  (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-js-mode
    "if" #'cb-flow-insert-flow-annotation)
  :bind (:map cb-web-js-mode-map
              ("C-c C-t" . cb-flow-type-at)))

(use-package cb-js-autoinsert
  :defer t
  :after autoinsert
  :config
  (add-to-list 'auto-insert-alist
               '((cb-web-js-mode . "JavaScript") . cb-js-autoinsert-template-string))
  :defines (auto-insert-alist))

(use-package cb-html-autoinsert
  :defer t
  :after autoinsert
  :config
  (add-to-list 'auto-insert-alist
               '((cb-web-html-mode . "HTML") . cb-html-autoinsert-template-string))
  :defines (auto-insert-alist))

(use-package tern
  :defer t
  :functions (tern-mode)
  :commands (tern-find-definition tern-pop-find-definition)
  :init
  (add-hook 'cb-web-js-mode-hook #'tern-mode)
  :config
  (progn
    (unless (getenv "NODE_PATH")
      (setenv "NODE_PATH" "/usr/local/lib/node_modules"))

    (evil-define-key 'normal tern-mode-keymap
      (kbd "M-.") #'tern-find-definition
      (kbd "M-,") #'tern-pop-find-definition)))

(use-package company-tern
  :after cb-web-modes
  :config
  (progn
    (setq company-tern-meta-as-single-line t)
    (setq company-tern-property-marker " <p>")

    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-tern))))

(use-package web-beautify
  :commands (web-beautify-js web-beautify-html web-beautify-css)
  :init
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-js-mode  "=" #'web-beautify-js)
    (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-json-mode "=" #'web-beautify-js)
    (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-html-mode  "=" #'web-beautify-html)
    (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-css-mode "=" #'web-beautify-css)))

(use-package stylus-mode
  :mode ("\\.styl\\'" . stylus-mode)
  :preface
  (defun cb-web--set-stylus-vars ()
    (setq-local tab-width 2))
  :config
  (add-hook 'stylus-mode-hook #'cb-web--set-stylus-vars))

(provide 'cb-web-mode)

;;; cb-web-mode.el ends here
