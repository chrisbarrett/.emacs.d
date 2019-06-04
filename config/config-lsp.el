;;; config-lsp.el --- Configuration for Language Server Protocol client  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar lsp-dockerfile-server "docker-langserver")

(use-package lsp-mode
  :straight t
  :defer t
  :config
  (general-setq lsp-eldoc-render-all nil
                lsp-inhibit-message t
                lsp-highlight-symbol-at-point nil)
  :hook ((c-mode-common . lsp)
         (dockerfile-mode . lsp)
         (go . lsp)
         (groovy . lsp)
         (js-mode . lsp)
         (python . lsp)
         (rust-mode . lsp)
         (sh-mode . lsp))
  :preface
  (defun config-lsp--setup-buffer ()
    (setq-local evil-lookup-func #'lsp-describe-thing-at-point)

    ;; Use server highlighting.
    (when (gethash "documentHighlightProvider" (lsp--server-capabilities))
      (highlight-thing-mode -1))

    ;; Format on save.
    (when (gethash "documentFormattingProvider" (lsp--server-capabilities))
      (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

  :init
  (progn
    (setq lsp-prefer-flymake nil)
    (setq lsp-enable-on-type-formatting nil)
    (setq lsp-session-file (f-join paths-cache-directory "lsp-session-v1")))

  :config
  (progn
    ;; HACK: Load this early so that patches work correcly.
    (when (and lsp-auto-configure lsp-auto-require-clients)
      (require 'lsp-clients))

    (add-hook 'lsp-after-open-hook #'config-lsp--setup-buffer)

    (define-key lsp-mode-map (kbd "C-c SPC") #'lsp-execute-code-action)

    ;; Add custom LSP clients.

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection (format "%s --stdio" lsp-dockerfile-server))
                      :major-modes '(dockerfile-mode)
                      :server-id 'dockerfile))))

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
    (defun config-lsp-toggle-ui-overlays (&optional should-enable)
      (interactive (list (not (bound-and-true-p lsp-ui-mode))))
      (cond
       (should-enable
        (lsp-ui-mode +1)
        (eldoc-mode -1))
       (t
        (lsp-ui-mode -1)
        (eldoc-mode +1))))

    (defun config-lsp--configure-ui ()
      (config-lsp-toggle-ui-overlays t)
      (lsp-ui-flycheck-enable t)))
  :init
  (progn
    (require 'lsp-ui-flycheck)
    (with-eval-after-load 'lsp-mode
      (define-key lsp-mode-map (kbd "C-c u") #'config-lsp-toggle-ui-overlays)))
  :config
  (progn
    (add-hook 'lsp-after-open-hook #'config-lsp--configure-ui)
    (general-setq lsp-ui-sideline-enable t
                  lsp-ui-sideline-show-code-actions nil
                  lsp-ui-sideline-show-flycheck nil
                  lsp-ui-doc-enable nil)

    (define-key lsp-ui-mode-map (kbd "C-c C-c") #'lsp-goto-type-definition)
    (define-key lsp-ui-mode-map (kbd "C-c i") #'lsp-goto-implementation)
    (define-key lsp-ui-mode-map [remap evil-lookup] #'lsp-describe-thing-at-point)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(provide 'config-lsp)

;;; config-lsp.el ends here
