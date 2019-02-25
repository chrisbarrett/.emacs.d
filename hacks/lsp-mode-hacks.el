;;; lsp-mode-hacks.el --- Hacks for lsp-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature lsp-mode)


(with-eval-after-load 'lsp-clients
  (with-no-warnings
    (el-patch-defun lsp-clients-flow-activate-p (file-name major-mode)
      "Checks if the Flow language server should be enabled for a
particular FILE-NAME and MAJOR-MODE."
      (and (derived-mode-p (el-patch-add 'web-js-base-mode 'web-js-mode 'web-ts-mode) 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode)
           (lsp-clients-flow-project-p file-name)
           (lsp-clients-flow-tag-present-p file-name)))

    (el-patch-defun lsp-typescript-javascript-tsx-jsx-activate-p (filename major-mode)
      "Checks if the javascript-typescript language server should be enabled
based on FILE-NAME and MAJOR-MODE"
      (or (member major-mode '((el-patch-add web-js-base-mode web-js-mode web-ts-mode) typescript-mode typescript-tsx-mode js-mode js2-mode rjsx-mode))
          (and (eq major-mode 'web-mode)
               (or (string-suffix-p ".tsx" filename t)
                   (string-suffix-p ".jsx" filename t)))))))

(provide 'lsp-mode-hacks)

;;; lsp-mode-hacks.el ends here
