;;; lsp-mode-hacks.el --- Hacks for lsp-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(autoload 'lsp-clients-flow-project-p "lsp-clients")
(autoload 'lsp-clients-flow-tag-present-p "lsp-clients")

(el-patch-feature lsp-mode)


(with-eval-after-load 'lsp-clients
  (el-patch-defun lsp-clients-flow-activate-p (file-name (el-patch-swap major-mode _mode))
    "Checks if the Flow language server should be enabled for a
particular FILE-NAME and MAJOR-MODE."
    (and (derived-mode-p (el-patch-add 'web-js-base-mode 'web-js-mode 'web-ts-mode) 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode)
         (lsp-clients-flow-project-p file-name)
         (lsp-clients-flow-tag-present-p file-name)))

  (el-patch-defun lsp-typescript-javascript-tsx-jsx-activate-p (filename (el-patch-swap major-mode mode))
    "Checks if the javascript-typescript language server should be enabled
based on FILE-NAME and MAJOR-MODE"
    (or (member (el-patch-swap major-mode mode) '((el-patch-add web-js-base-mode web-js-mode web-ts-mode) typescript-mode typescript-tsx-mode js-mode js2-mode rjsx-mode))
        (and (eq (el-patch-swap major-mode mode) 'web-mode)
             (or (string-suffix-p ".tsx" filename t)
                 (string-suffix-p ".jsx" filename t))))))

(provide 'lsp-mode-hacks)

;;; lsp-mode-hacks.el ends here
