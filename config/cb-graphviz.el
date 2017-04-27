;;; cb-graphviz.el --- Configuration for Graphviz.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package graphviz-dot-mode
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


(provide 'cb-graphviz)

;;; cb-graphviz.el ends here
