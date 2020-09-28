;;; config-js.el --- Configuration for JavaScript editing.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(cl-eval-when (compile)
  (require 'flycheck))

(require 'dash)
(require 'general)
(require 'paths)

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
  :mode ("\\.[cm]?jsx?\\'" . js-mode)
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

;; `tide' is a TS development environment. It's currently more enjoyable to use than the LSP.

(use-package tide
  :after (company flycheck)
  :preface
  (defun config-js--maybe-enable-tide ()
    (unless (derived-mode-p 'json-mode)
      (require 'tide)
      (tide-setup)
      (tide-hl-identifier-mode +1)))

  :hook ((typescript-mode . config-js--maybe-enable-tide)
         (js-mode . config-js--maybe-enable-tide)
         (before-save . tide-format-before-save))
  :general
  (:states '(normal insert emacs) :keymaps 'tide-mode-map
   "M-." 'tide-jump-to-definition)
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) t)))

(provide 'config-js)

;;; config-js.el ends here
