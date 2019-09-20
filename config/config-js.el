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
           (js-js-tmpdir (f-join paths-cache-directory "js")))
  :config
  (progn
    (setq js--prettify-symbols-alist '(("function" . ?ƒ)))
    (with-eval-after-load 'lsp-mode
      (add-to-list 'lsp-language-id-configuration '(js-mode . "javascript")))))

(use-package css-mode
  :defer t
  :custom
  ((css-indent-offset 2)))

;; `emmet-mode' provides support for expandable HTML/JSX snippets.

(use-package emmet-mode
  :straight t
  :general (:states '(normal insert)
            :keymaps 'js-mode-map
            "C-M-n" 'emmet-next-edit-point
            "C-M-p" 'emmet-prev-edit-point
            "C-t" 'emmet-expand-line)
  :hook (js-mode . emmet-mode)
  :config
  (progn
    (setq emmet-expand-jsx-className? t)
    (setq emmet-move-cursor-between-quotes t)))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
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
