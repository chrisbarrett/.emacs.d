;;; config-langs.el --- Configuration for simple language packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode)
  :preface
  (defun config-langs--suppress-final-newline ()
    (setq-local require-final-newline nil))
  :config (add-hook 'csv-mode-hook #'config-langs--suppress-final-newline))

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

(use-package rmsbolt
  :straight
  (:host gitlab :repo "jgkamat/rmsbolt")
  :preface
  (defun config-langs--override-haskell-compile-command (f &rest args)
    (let ((rmsbolt-command
           (if (locate-dominating-file default-directory "stack.yaml")
               "stack ghc --"
             "ghc")))
      (apply f args)))

  :config
  (advice-add 'rmsbolt--hs-compile-cmd :around #'config-langs--override-haskell-compile-command))

(provide 'config-langs)

;;; config-langs.el ends here
