;;; config-java.el --- Configuration for Java  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-major-mode-hydra)
(autoload 'indent-buffer "config-editing")

(with-no-warnings
  (cb-major-mode-hydra-define java-mode
    "Refactor"
    (("ri" lsp-java-add-import "add import")
     ("rl" lsp-java-extract-to-local-variable "extract local var")
     ("rm" lsp-java-extract-method "extract method")
     ("ro" lsp-java-organize-imports "organise imports")
     ("rr" lsp-rename "rename"))
    ""
    (("ap" lsp-java-create-parameter "add parameter")
     ("af" lsp-java-create-field "add field")
     ("ax" lsp-java-add-unimplemented-methods "add method stubs"))))



(use-package java-mode
  :defer t
  :config
  (when (bound-and-true-p java-mode-map)
    (define-key java-mode-map [remap c-fill-paragraph] #'indent-buffer)))

(provide 'config-java)

;;; config-java.el ends here
