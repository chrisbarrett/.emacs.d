;;; cb-ide.el --- Advanced language support.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)

(use-package treesit
  :custom
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(use-package eglot
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-extend-to-xref t)
  :general
  (:keymaps 'eglot-mode-map
   "C-c C-r" 'eglot-rename
   "M-RET" 'eglot-code-actions)
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))

  ;; Set which modes have eglot enabled automatically
  :preface
  (defun config-eglot--supported-modes ()
    (->> (bound-and-true-p eglot-server-programs)
         (seq-map #'car)
         (-flatten)
         (seq-filter (lambda (it)
                       (and (symbolp it) (not (keywordp it)))))))

  (defun config-eglot-enable-if-lsp-exists-p ()
    (require 'eglot)
    (when (seq-contains-p (config-eglot--supported-modes) major-mode)
      (eglot-ensure)))
  :hook
  ((prog-mode text-mode) . config-eglot-enable-if-lsp-exists-p))

(use-package eglot-x :ensure t :demand t :after eglot
  :general
  (:keymaps 'eglot-mode-map "C-c ?" 'eglot-x-find-refs))

(use-package eldoc :hook (emacs-lisp-mode . eldoc-mode)
  :custom
  (eldoc-idle-delay 0.2)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :preface
  (define-advice eldoc-doc-buffer (:after (&rest _args) select-window)
    (select-window (get-buffer-window eldoc--doc-buffer)))

  (define-advice eldoc--format-doc-buffer (:after (&rest _) page-break-lines)
    (when (and (buffer-live-p eldoc--doc-buffer) (fboundp 'page-break-lines-mode))
      (with-current-buffer eldoc--doc-buffer
        (page-break-lines-mode +1)))))

(use-package flymake :hook ((prog-mode text-mode) . flymake-mode)
  :general
  (:keymaps 'flymake-mode-map
   "M-p" 'flymake-goto-prev-error
   "M-n" 'flymake-goto-next-error))

(use-package xref
  :general
  (:keymaps '(prog-mode-map ielm-map) :states '(normal visual insert)
   "M-." 'xref-find-definitions))

(provide 'cb-ide)

;;; cb-ide.el ends here
