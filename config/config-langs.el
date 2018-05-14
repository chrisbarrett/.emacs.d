;;; config-langs.el --- Configuration for simple language packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))

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
  :mode
  (("\\.dot\\'" . graphviz-dot-mode)
   (("\\.gv\\'" . graphviz-dot-mode)))
  :config
  (progn
    (let ((keymap (with-no-warnings graphviz-dot-mode-map)))
      (define-key keymap (kbd "M-q") 'graphviz-dot-indent-graph)
      (define-key keymap (kbd "{") nil)
      (define-key keymap (kbd "}") nil))))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode))

(use-package yaml-mode
  :straight t
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)
  :preface
  (defun config-langs--disable-autofill ()
    (auto-fill-mode -1))
  :config
  (add-hook 'yaml-mode-hook #'config-langs--disable-autofill))

(provide 'config-langs)

;;; config-langs.el ends here
