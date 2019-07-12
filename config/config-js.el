;;; config-js.el --- Configuration for JavaScript editing.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'lsp-mode))

(require 'general)
(require 'paths)



;; `js' is the Emacs built-in javascript mode. However, we actually want to use
;; the newer version that supports JSX, copied from Emacs master.

(use-package js
  :mode ("\\.jsx?\\'" . js-mode)
  :load-path paths-lisp-directory
  :custom ((js-indent-level 2)
           (js-switch-indent-offset 2)
           (js--prettify-symbols-alist '(("function" . ?ƒ)))
           (js-js-tmpdir (f-join paths-cache-directory "js")))
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(js-mode . "javascript"))))

(use-package typescript-mode
  :straight t
  :custom ((typescript-indent-level 2)))

(use-package nvm
  :straight t
  :custom ((nvm-dir "~/.config/nvm"))

  :functions (nvm-use-for-buffer)
  :preface
  (defun config-js-maybe-use-nvm ()
    (when (locate-dominating-file default-directory ".nvmrc")
      (nvm-use-for-buffer)
      t))
  :hook ((js-mode . config-js-maybe-use-nvm)
         (typescript-mode . config-js-maybe-use-nvm)))


(provide 'config-js)

;;; config-js.el ends here
