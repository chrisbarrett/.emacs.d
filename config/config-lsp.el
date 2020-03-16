;;; config-lsp.el --- Configuration for Language Server Protocol client  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package lsp-mode
  :straight t
  :defer t
  :custom
  ((lsp-auto-execute-action nil)
   (lsp-diagnostic-package :flycheck)
   (lsp-before-save-edits t)
   (lsp-eldoc-render-all nil)
   (lsp-enable-on-type-formatting nil)
   (lsp-enable-semantic-highlighting t)
   (lsp-restart 'auto-restart)
   (lsp-session-file (f-join paths-cache-directory "lsp-session-v1")))

  :hook (prog-mode . lsp-deferred)

  :preface
  (defun config-lsp--setup-buffer ()
    (setq-local evil-lookup-func #'lsp-describe-thing-at-point)
    (setq-local company-minimum-prefix-length 1)
    (setq-local company-idle-delay 0.0)

    ;; Use server highlighting.
    (when (gethash "documentHighlightProvider" (lsp--server-capabilities))
      (highlight-thing-mode -1))

    ;; KLUDGE: lsp-eslint has a variable, `lsp-eslint-auto-fix-on-save', for
    ;; enabling format on save. But it's not currently used. :'(
    (when (derived-mode-p 'js-mode)
      (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes nil t)))

  :config
  (progn
    ;; HACK: Load this early so that patches work correcly.
    (when lsp-auto-configure
      (require 'lsp-clients))

    (add-hook 'lsp-after-open-hook #'config-lsp--setup-buffer)

    (define-key lsp-mode-map (kbd "S-<return>") #'lsp-execute-code-action)))

(use-package dap-mode
  :straight t
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :init
  (use-package dap-node :hook (js-mode . dap-node-setup))
  :preface
  (defvar config-lsp--dap-cache-dir (expand-file-name "dap" paths-cache-directory))
  :init
  (progn
    (f-mkdir config-lsp--dap-cache-dir)
    (general-setq dap-utils-extension-path (expand-file-name "extensions" config-lsp--dap-cache-dir)))
  :config
  (general-setq dap-breakpoints-file (expand-file-name "breakpoints" config-lsp--dap-cache-dir)))

(use-package lsp-java
  :straight t
  :defer t
  :preface
  (defun config-lsp--java-mode-setup ()
    (setq-local company-backends '(company-lsp)))
  :init
  (add-hook 'java-mode-hook #'config-lsp--java-mode-setup)
  :config
  (let ((cache-dir (f-join paths-cache-directory "lsp-java")))
    (f-mkdir cache-dir)
    (general-setq lsp-java-server-install-dir (f-join cache-dir "eclipse.jdt.ls/server/")
                  lsp-java-workspace-dir (f-join cache-dir "workspace/")
                  lsp-java-workspace-cache-dir (f-join cache-dir "workspace" ".cache/"))))

(use-package company-lsp
  :after (:all company lsp-mode)
  :straight t
  :defer t
  :config
  (general-setq company-lsp-cache-candidates t
                company-lsp-async t))

(use-package lsp-ui
  :straight t
  :defer t
  :after lsp-mode
  :preface
  (progn
    (defun config-lsp-toggle-ui-overlays ()
      (interactive)
      (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      (eldoc-mode (if lsp-ui-sideline-show-hover -1 +1))
      (lsp-ui-sideline))

    (defun config-lsp--configure-ui ()
      (lsp-ui-sideline-mode +1)
      (lsp-ui-flycheck-enable t)))
  :init
  (progn
    (require 'lsp-ui-flycheck)
    (with-eval-after-load 'lsp-mode
      (define-key lsp-mode-map (kbd "C-c u") #'config-lsp-toggle-ui-overlays)))
  :config
  (progn
    (add-hook 'lsp-after-open-hook #'config-lsp--configure-ui)
    (general-setq lsp-ui-sideline-show-code-actions nil
                  lsp-ui-sideline-show-hover nil
                  lsp-ui-doc-enable nil)

    (define-key lsp-ui-mode-map (kbd "C-c C-c") #'lsp-goto-type-definition)
    (define-key lsp-ui-mode-map (kbd "C-c i") #'lsp-goto-implementation)
    (define-key lsp-ui-mode-map [remap evil-lookup] #'lsp-describe-thing-at-point)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(use-package lsp-treemacs
  :straight t
  :after (:all treemacs lsp-mode)
  :config (lsp-treemacs-sync-mode))

(provide 'config-lsp)

;;; config-lsp.el ends here
