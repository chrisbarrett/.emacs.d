;;; config-js.el --- Configuration for JavaScript editing.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'paths)
(require 'lsp-mode-hacks)

(defgroup config-js nil
  "JS configuration"
  :group 'languages
  :prefix "config-js-")

(defcustom config-js-ignored-error-ids nil
  "A list of error codes from the language server to ignore.

Needed because the typescript language server lacks a nice way to
disable code actions selectively.

Expected to be set as a dir-local variable."
  :group 'config-js
  :safe (lambda (xs) (-all-p 'cl-plusp xs))
  :type '(list numberp))

(defun config-js--filter-flycheck-errors (err)
  (if (derived-mode-p 'js-mode)
      (not (memq (flycheck-error-id err) config-js-ignored-error-ids))
    t))

(add-hook 'config-lsp-error-filter-functions #'config-js--filter-flycheck-errors)

(defun js-sort-imports-by-path (beg end)
  "Sort Common JS imports between BEG and END."
  (interactive "r")
  (sort-regexp-fields nil
                      "^.*$"
                      (rx (or (and "require" (* space) "(" )
                              (and "import" (*? nonl) "from"))
                          (* space)(any "\"'") (* nonl))
                      beg
                      end))

;; `js' is the Emacs built-in JavaScript mode.
(use-package js
  :mode ("\\.jsx?\\'" . js-mode)
  :custom ((js-indent-level 2)
           (js-switch-indent-offset 2)
           (js-js-tmpdir (f-join paths-cache-directory "js")))
  :config
  (setq js--prettify-symbols-alist '(("function" . ?ƒ))))

;; `css-mode' is the built-in CSS mode.
(use-package css-mode
  :defer t
  :custom
  ((css-indent-offset 2)))

;; `emmet-mode' provides support for expandable HTML/JSX snippets.
(use-package emmet-mode
  :general (:states '(normal insert)
            :keymaps '(js-mode-map typescript-mode-map)
            "C-M-n" 'emmet-next-edit-point
            "C-M-p" 'emmet-prev-edit-point
            "C-t" 'emmet-expand-line)
  :hook ((js-mode . emmet-mode)
         (typescript-mode . emmet-mode))
  :config
  (progn
    (setq emmet-expand-jsx-className? t)
    (setq emmet-move-cursor-between-quotes t)))

;; `typescript-mode' adds support for editing typescript files.
(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :custom ((typescript-indent-level 2)))

;; `nvm' teaches Emacs to update the exec-path according to the current nvm
;; profile.
(use-package nvm
  :when (file-directory-p "~/.config/nvm")
  :custom ((nvm-dir "~/.config/nvm"))
  :functions (nvm-use-for-buffer)
  :preface
  (defun config-js-maybe-use-nvm ()
    ;; If there's a shell.nix, assume we're using Lorri to manage which Node+NPM
    ;; programs are used.
    (unless (locate-dominating-file default-directory "shell.nix")
      (when (locate-dominating-file default-directory ".nvmrc")
        (nvm-use-for-buffer)
        t)))
  :hook ((js-mode . config-js-maybe-use-nvm)
         (org-mode . config-js-maybe-use-nvm)
         (typescript-mode . config-js-maybe-use-nvm)))

(provide 'config-js)

;;; config-js.el ends here
