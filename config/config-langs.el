;;; config-langs.el --- Configuration for simple language packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'general)
  (require 'use-package))

(cl-eval-when (compile)
  (require 'company-lsp nil t))

(require 'major-mode-hydra)

(autoload 'display-buffer-fullframe "display-buffer-fullframe")
(autoload 'indent-buffer "config-editing")



(use-package apiwrap)

(use-package poporg
  :commands (poporg-dwim))

(use-package csv-mode
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
  :mode ("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode)
  :interpreter ("groovy" . groovy-mode))

(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         (("\\.gv\\'" . graphviz-dot-mode)))
  :general (:keymaps 'graphviz-dot-mode-map "M-q" #'graphviz-dot-indent-graph)
  :init (general-unbind :keymaps 'graphviz-dot-mode-map "{" "}"))

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package terraform-mode
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode))

(use-package yaml-mode
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)
  :general
  (:states '(normal insert) :keymaps 'yaml-mode-map
   [backtab] 'yaml-indent-line)
  :preface
  (defun config-langs--disable-autofill ()
    (auto-fill-mode -1))
  :config
  (add-hook 'yaml-mode-hook #'config-langs--disable-autofill))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(use-package highlight-indent-guides
  :hook ((python-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode)))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (general-setq lua-indent-level 2))

(use-package lsp-lua-emmy
  :after lua-mode
  :custom
  ((lsp-lua-emmy-jar-path (getenv "NIX_EMACS_EMMY_LUA_JAR"))
   (lsp-lua-emmy-java (expand-file-name "bin/java" (getenv "JAVA_HOME"))))
  :config
  (with-eval-after-load 'company-lsp
    (add-to-list 'company-lsp-filter-candidates '(lsp-emmy-lua . t))))

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :general (:states '(motion normal) :keymaps 'pdf-view-mode-map
            "n" #'pdf-view-next-page
            "N" #'pdf-view-previous-page
            "p" #'pdf-view-previous-page)
  :custom
  ((pdf-view-display-size 'fit-page)
   (pdf-annot-activate-created-annotations t))
  :init
  (progn
    (use-package pdf-history
      :commands (pdf-history-minor-mode))
    (use-package pdf-occur
      :commands (pdf-occur-global-minor-mode)))
  :config
  (progn
    (require 'pdf-sync)
    (require 'pdf-links)
    (require 'pdf-outline)
    (require 'pdf-history)
    (require 'pdf-annot)
    (pdf-tools-install)))

(use-package graphql-mode
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package pass
  :commands (pass)
  :general
  (:states '(normal) :keymaps 'pass-view-mode-map "q" #'kill-this-buffer)
  (:states '(normal) :keymaps 'pass-mode-map
   "u" #'pass-copy-username
   "U" #'pass-copy-url
   "J" #'pass-goto-entry
   "y" #'pass-copy-password
   "f" #'pass-copy-field)
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Password-Store*" eos)
                 (display-buffer-reuse-window
                  display-buffer-fullframe)
                 (reusable-frames . visible))))

(provide 'config-langs)

;;; config-langs.el ends here
