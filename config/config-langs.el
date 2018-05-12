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
  (defun config-csv--suppress-final-newline ()
    (setq-local require-final-newline nil))
  :config (add-hook 'csv-mode-hook #'config-csv--suppress-final-newline))

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
      (define-key keymap (kbd "}") nil))

    (spacemacs-keys-set-leader-keys-for-major-mode 'graphviz-dot-mode
      "c" 'compile
      "p" 'graphviz-dot-preview
      "," 'graphviz-dot-preview)))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package terraform-mode
  :straight t
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode))

(provide 'config-langs)

;;; config-langs.el ends here
