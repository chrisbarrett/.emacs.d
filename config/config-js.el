;;; config-js.el --- Configuration for JavaScript editing.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'paths)



(use-package js
  :mode ("\\.jsx?\\'" . js-mode)
  :load-path paths-lisp-directory
  :config
  (progn
    (with-eval-after-load 'lsp-mode
      (add-to-list 'lsp-language-id-configuration '(js-mode . "javascript")))
    (general-setq js-indent-level 2
                  js-switch-indent-offset 2
                  js--prettify-symbols-alist '(("function" . ?ƒ)))
    (custom-theme-set-faces
     'user
     '(js2-external-variable ((t (:inherit font-lock-keyword-face))))
     '(js2-function-call ((t (:inherit default :foreground nil))))
     '(js2-object-property-access ((t (:inherit js2-object-property :foreground nil :italic nil))))
     '(js2-object-property ((t (:italic t :foreground nil)))))))

(use-package nvm
  :straight t
  :hook (js-mode . config-js-maybe-use-nvm)
  :functions (nvm-use-for-buffer)
  :custom ((nvm-dir "~/.config/nvm"))
  :preface
  (defun config-js-maybe-use-nvm ()
    (when (locate-dominating-file default-directory ".nvmrc")
      (nvm-use-for-buffer)
      t)))


(provide 'config-js)

;;; config-js.el ends here
