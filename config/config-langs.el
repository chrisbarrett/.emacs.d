;;; config-langs.el --- Configuration for simple language packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)

(autoload 'display-buffer-fullframe "display-buffer-fullframe")
(autoload 'indent-buffer "config-editing")



(use-package apiwrap
  :straight t)

(use-package poporg
  :straight t
  :commands (poporg-dwim))

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode)
  :preface
  (defun config-langs--suppress-final-newline ()
    (setq-local require-final-newline nil))
  :config (add-hook 'csv-mode-hook #'config-langs--suppress-final-newline))

(use-package make-mode
  :preface
  (defun config-langs--set-up-makefile-mode ()
    (setq-local tab-width 8)
    (setq-local indent-tabs-mode t))
  :init
  (add-hook 'makefile-mode-hook #'config-langs--set-up-makefile-mode))

(use-package java-mode
  :defer t
  :config
  (progn
    (when (bound-and-true-p java-mode-map)
      (define-key java-mode-map [remap c-fill-paragraph] #'indent-buffer))

    (major-mode-hydra-define java-mode nil
      ("Imports"
       (("ri" lsp-java-add-import "add import")
        ("ro" lsp-java-organize-imports "organise imports"))
       "Refactor"
       (("rl" lsp-java-extract-to-local-variable "extract local var")
        ("rm" lsp-java-extract-method "extract method")
        ("rr" lsp-rename "rename")
        ("ap" lsp-java-create-parameter "add parameter")
        ("af" lsp-java-create-field "add field")
        ("ax" lsp-java-add-unimplemented-methods "add method stubs"))))))

(use-package groovy-mode
  :straight t
  :mode ("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode)
  :interpreter ("groovy" . groovy-mode))

(use-package graphviz-dot-mode
  :straight t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         (("\\.gv\\'" . graphviz-dot-mode)))
  :general (:keymaps 'graphviz-dot-mode-map "M-q" #'graphviz-dot-indent-graph)
  :init (general-unbind :keymaps 'graphviz-dot-mode-map "{" "}"))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode))

(use-package yaml-mode
  :straight t
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)
  :general
  (:states '(normal insert) :keymaps 'yaml-mode-map
   [backtab] 'yaml-indent-line)
  :preface
  (defun config-langs--disable-autofill ()
    (auto-fill-mode -1))
  :config
  (add-hook 'yaml-mode-hook #'config-langs--disable-autofill))

(use-package lua-mode
  :straight t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (general-setq lua-indent-level 2))

(use-package pdf-tools
  :straight t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config
  (progn
    (pdf-tools-install)
    (general-setq pdf-view-display-size 'fit-page
                  pdf-annot-activate-created-annotations t)))

(use-package graphql-mode
  :straight t
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package pass
  :straight (:host github :repo "NicolasPetton/pass")
  :commands (pass)
  :general
  (:states '(normal) :keymaps 'pass-view-mode-map "q" #'kill-this-buffer)
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Password-Store*" eos)
                 (display-buffer-reuse-window
                  display-buffer-fullframe)
                 (reusable-frames . visible))))

(provide 'config-langs)

;;; config-langs.el ends here
