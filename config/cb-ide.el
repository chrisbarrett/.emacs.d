;;; cb-ide.el --- Advanced language support.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(use-package eglot
  :custom
  (eglot-extend-to-xref t)
  :general
  (:keymaps 'eglot-mode-map
   "C-c C-r" 'eglot-rename
   "M-RET" 'eglot-code-actions)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs '((yaml-ts-mode yaml-mode)
                                        "yaml-language-server" "--stdio"
                                        :initializationOptions
                                        (:keyOrdering :json-false)))


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
  (prog-mode . config-eglot-enable-if-lsp-exists-p)
  (text-mode . config-eglot-enable-if-lsp-exists-p))

(use-package eglot-x
  :after eglot
  :demand t
  :general
  (:keymaps 'eglot-mode-map "C-c ?" 'eglot-x-find-refs))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
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

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  (text-mode . flymake-mode)
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
