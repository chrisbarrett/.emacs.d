;;; cb-lang-typescript.el --- <enter description here>  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)

(defun cb-readonly-if-in-node-modules ()
  (when (string-match-p  "/node_modules/" default-directory)
    (read-only-mode +1)))

(add-hook 'find-file-hook #'cb-readonly-if-in-node-modules)

(use-package typescript-ts-mode
  :mode
  ("\\.[cm]?ts\\'" . typescript-ts-mode)
  :config
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))

(use-package js
  :mode ("\\.[cm]?jsx?\\'" . js-ts-mode)
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  (js-js-tmpdir (no-littering-expand-var-file-name "js"))
  :init
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode)))

(use-package prettier
  :init
  (add-hook 'typescript-ts-mode-hook 'prettier-mode 10)
  :preface
  (define-advice prettier--find-node (:around (fn server-id) inject-via-envvar)
    (pcase server-id
      ('local
       (getenv "NIX_EMACS_NODE_PROGRAM"))
      (_
       (funcall fn server-id)))))

(provide 'cb-lang-typescript)

;;; cb-lang-typescript.el ends here
