;;; config-js.el --- Configuration for JavaScript editing.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))



(use-package js
  :defer t
  :config
  (progn
    (general-setq js--prettify-symbols-alist '(("function" . ?ƒ)))
    (custom-theme-set-faces
     'user
     '(js2-external-variable ((t (:inherit font-lock-keyword-face))))
     '(js2-function-call ((t (:inherit default :foreground nil))))
     '(js2-object-property-access ((t (:inherit js2-object-property :foreground nil :italic nil))))
     '(js2-object-property ((t (:italic t :foreground nil)))))))

(use-package rjsx-mode
  :straight t
  :mode ("\\.jsx?\\'" . rjsx-mode))

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
