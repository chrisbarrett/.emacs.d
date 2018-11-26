;;; config-lsp.el --- Configuration for Language Server Protocol client  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar lsp-dockerfile-server "docker-langserver")

(use-package lsp-mode
  :straight t
  :config
  (general-setq lsp-eldoc-render-all nil
                lsp-inhibit-message t
                lsp-highlight-symbol-at-point nil)
  :config
  (progn
    (lsp-define-stdio-client lsp-dockerfile "Docker" (lambda () default-directory) (list lsp-dockerfile-server "--stdio"))
    (add-hook 'dockerfile-mode-hook #'lsp-dockerfile-enable)))

(use-package company-lsp
  :after company
  :straight t
  :config
  (general-setq company-lsp-cache-candidates t
                company-lsp-async t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (progn
    (general-setq lsp-ui-sideline-enable nil
                  lsp-ui-doc-enable nil)

    (define-key lsp-ui-mode-map (kbd "C-c C-c") #'lsp-goto-type-definition)
    (define-key lsp-ui-mode-map (kbd "C-c i") #'lsp-goto-implementation)
    (define-key lsp-ui-mode-map [remap evil-lookup] #'lsp-describe-thing-at-point)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(provide 'config-lsp)

;;; config-lsp.el ends here
