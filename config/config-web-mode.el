;;; config-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'config-etags)
  (require 'use-package))

(require 'cb-major-mode-hydra)
(require 'f)
(require 'nvm-hacks)
(require 's)
(require 'subr-x)

(autoload 'flycheck-add-mode "flycheck")
(autoload 'projectile-project-p "projectile")



(dolist (mode '(web-js-mode web-ts-mode))
  (eval `(cb-major-mode-hydra-define ,mode
           "Refactor"
           (("ro" js-refactor-commands-organize-imports "organise imports")
            ("ra" js-refactor-commands-align-object-literal-values "align object values")
            ("re" js-refactor-commands-expand-comma-bindings "expand comma bindings")
            ("rr" lsp-rename "rename...")
            ("rs" js-refactor-commands-toggle-sealed-object-type "toggle sealed type"))
           "Test"
           (("t" js-test-commands-test-this-file-dwim "test file")))))



(use-package web-mode
  :straight t
  :defer t
  :mode (("\\.php\\'" . web-mode))
  :preface
  (progn
    (autoload 'sp-local-pair "smartparens")
    (autoload 'projectile-project-p "projectile")

    (defun config-web--node-modules-bin-dir ()
      (when-let* ((root (projectile-project-p))
                  (dir (f-join root "node_modules" ".bin")))
        (when (f-dir? dir)
          dir)))

    (defun config-web--add-node-modules-bin-to-exec-path ()
      (when-let* ((bin (config-web--node-modules-bin-dir)))
        (setq-local exec-path (cons bin exec-path)))))

  :custom
  (web-mode-extra-keywords '(("javascript" . ("declare" "module" "type"))))

  :config
  (progn
    (general-setq
     web-mode-code-indent-offset 2
     web-mode-css-indent-offset 2
     web-mode-markup-indent-offset 2
     web-mode-enable-auto-quoting nil)

    (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
    (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))

    ;; Disable web-mode-reload binding
    (define-key web-mode-map (kbd "C-c C-r") nil)

    ;; Use line comments when commenting in JS.

    (setf (cdr (assoc "javascript" web-mode-comment-formats)) "//")

    ;; Change default indentation behaviour.

    (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-ternary" web-mode-indentation-params)) nil)

    ;; Treat es6 files as JS files.

    (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
    (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))
    (add-to-list 'web-mode-content-types '("jsx" . "\\.js\\.snap\\'"))

    ;; Dim Promise .then method.

    (add-to-list 'web-mode-javascript-font-lock-keywords
                 `(,(rx (? "." (* space)) symbol-start "then" symbol-end) 0 'web-mode-keyword-face))

    ;; Dim parentheses.

    (add-to-list 'web-mode-javascript-font-lock-keywords
                 `(,(rx (any "(){}?:;,")) 0 'parenthesis))

    ;; Run programs out of node_bin

    (add-hook 'web-mode-hook #'config-web--add-node-modules-bin-to-exec-path)))

(use-package rainbow-mode
  :straight t
  :hook ((web-mode . rainbow-mode)
         (emacs-lisp-mode . rainbow-mode)))

(use-package web-mode-submodes
  :defer t
  :mode (("\\.json\\'" . web-json-mode)
         ("\\.eslintrc\\'" . web-json-mode)
         ("\\.babelrc\\'" . web-json-mode)
         ("\\.es6\\'"  . web-js-mode)
         ("\\.js\\.snap\\'"  . web-js-snap-mode)
         ("\\.tsx?\\'"  . web-ts-mode)
         ("\\.jsx?\\'" . web-js-mode)
         ("\\.css\\'"  . web-css-mode)
         ("\\.mustache\\'"  . web-mustache-mode)
         ("\\.scss\\'"  . web-css-mode)
         ("\\.html\\'" . web-html-mode)
         ("\\.avsc\\'" . avro-mode)
         ("\\.avro\\'" . avro-mode))
  :preface
  (progn
    (defun config-web--choose-mode (&rest _)
      (catch 'stop
        (-each `(("\\.json\\'" . web-json-mode)
                 ("\\.eslintrc\\'" . web-json-mode)
                 ("\\.babelrc\\'" . web-json-mode)
                 ("\\.es6\\'"  . web-js-mode)
                 ("\\.js\\.snap\\'"  . web-js-snap-mode)
                 ("\\.tsx?\\'"  . web-ts-mode)
                 ("\\.jsx?\\'" . web-js-mode)
                 (,(rx bos "*Org Src" (+? nonl) "[ js ]*" eos) . web-js-mode))
          (-lambda ((regex . mode))
            (if (string-match-p regex (buffer-name))
                (progn
                  (funcall mode)
                  (throw 'stop nil))
              (web-mode))))))

    (defun config-web--enable-readonly-mode-in-node-modules ()
      (when (and (buffer-file-name)
                 (string-match-p (rx "/node_modules/") (buffer-file-name)))
        (read-only-mode +1))))
  :init
  (progn
    (add-hook 'find-file-hook #'config-web--enable-readonly-mode-in-node-modules)

    ;; HACK: Ensure these modes always override other web programming major modes.
    (advice-add 'js-mode :override #'config-web--choose-mode)
    (advice-add 'json-mode :override #'config-web--choose-mode)
    (advice-add 'typescript-mode :override #'config-web--choose-mode)
    (advice-add 'javascript-mode :override #'config-web--choose-mode)
    (advice-add 'js2-mode :override #'config-web--choose-mode)))

(use-package flycheck
  :defer t
  :commands (flycheck-select-checker)
  :functions (flycheck-add-next-checker flycheck-add-mode)
  :preface
  (defun config-web--disable-flycheck-for-node-modules ()
    (when (and (buffer-file-name)
               (s-contains-p "node_modules" (buffer-file-name))
               (boundp 'flycheck-checkers)
               (boundp 'flycheck-disabled-checkers))
      (let* ((js-checkers (seq-filter (lambda (checker)
                                        (string-prefix-p "javascript" (symbol-name checker)))
                                      flycheck-checkers))
             (updated (cl-union flycheck-disabled-checkers js-checkers)))
        (setq flycheck-disabled-checkers updated))))

  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)

    (add-hook 'web-js-mode-hook #'config-web--disable-flycheck-for-node-modules)
    (setq flycheck-html-tidy-executable (executable-find "tidy"))

    (flycheck-add-mode 'typescript-tslint 'web-ts-mode)
    (flycheck-add-mode 'javascript-eslint 'web-js-mode)
    (flycheck-add-mode 'javascript-jshint 'web-js-mode)
    (flycheck-add-mode 'javascript-standard 'web-js-mode)

    (flycheck-add-mode 'css-csslint 'web-css-mode)
    (flycheck-add-mode 'json-jsonlint 'web-json-mode)
    (flycheck-add-mode 'html-tidy 'web-html-mode)

    (with-eval-after-load 'lsp-ui
      (flycheck-add-next-checker 'lsp-ui 'javascript-eslint))))

(use-package emmet-mode
  :straight t
  :defer t
  :commands (emmet-mode emmet-expand-yas)
  :preface
  (progn
    (autoload 'sp-get-enclosing-sexp "smartparens")
    (autoload 'sp-up-sexp "smartparens")
    (autoload 'yas--templates-for-key-at-point "yasnippet")
    (autoload 'yas-expand "yasnippet")

    (defun config-web--set-jsx-classname-on ()
      (setq-local emmet-expand-jsx-className? t))

    (defun config-web--maybe-emmet-mode ()
      (cond
       ((derived-mode-p 'web-html-mode 'html-mode 'nxml-mode)
        (emmet-mode +1))
       ((equal web-mode-content-type "html")
        (emmet-mode +1))))

    (defun config-web--emmet-move-out-of-squares ()
      "Move point outside squares before expansion."
      (cl-flet ((sp-has-op? (op)
                            (-when-let ((&plist :op o) (sp-get-enclosing-sexp))
                              (equal op o)))
                (move-out-of-sexp ()
                                  (-when-let ((&plist :end end) (sp-get-enclosing-sexp))
                                    (goto-char end))))
        (cond
         ((sp-has-op? "[")
          (move-out-of-sexp))
         ((and (sp-has-op? "\"")
               (save-excursion
                 (sp-up-sexp)
                 (sp-has-op? "[")))
          (move-out-of-sexp)
          (move-out-of-sexp)))))

    (defun config-web-expand-snippet-then-emmet ()
      (interactive)
      (if (yas--templates-for-key-at-point)
          (call-interactively #'yas-expand)
        (config-web--emmet-move-out-of-squares)
        (emmet-expand-yas))))

  :init
  (progn
    (with-eval-after-load 'nxml-mode
      (define-key nxml-mode-map (kbd "<C-return>") #'emmet-expand-line))
    (with-eval-after-load 'web-mode
      (define-key web-mode-map (kbd "<C-return>") #'emmet-expand-line)))

  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (add-hook 'web-js-mode-hook #'config-web--set-jsx-classname-on)))

(use-package aggressive-indent
  :defer t
  :preface
  (defun config-web--in-flow-strict-object-type? ()
    (when (derived-mode-p 'web-js-mode)
      (-let [(depth start) (syntax-ppss)]
        (and (cl-plusp depth)
             (eq (char-after start) ?{)
             (eq (char-after (1+ start)) ?|)))))
  :config
  (progn
    (add-to-list 'aggressive-indent-dont-indent-if '(config-web--in-flow-strict-object-type?))
    (add-hook 'aggressive-indent-stop-here-hook #'config-web--in-flow-strict-object-type?)))

(use-package prettier-js
  :straight t
  :hook (web-mode . prettier-js-mode)
  :preface
  (progn
    (defvar prettier-js-inhibited-for-project nil)

    (put 'prettier-js-inhibited-for-project 'safe-local-variable 'symbolp)

    (defun config-web--child-file-of-node-modules-p ()
      (and (buffer-file-name) (string-match-p "/node_modules/" (buffer-file-name))))

    (defun config-web--maybe-inhibit-prettier (f &rest args)
      (when (derived-mode-p 'web-js-base-mode)
        (unless (or prettier-js-inhibited-for-project
                    (config-web--child-file-of-node-modules-p))
          (apply f args)))))
  :config
  (advice-add #'prettier-js :around #'config-web--maybe-inhibit-prettier))

(use-package compile
  :defer t
  :config
  (progn
    (defconst config-web--flow-error-rx
      (rx bol "Error:" (+ space)
          ;; Filename
          (group (+? nonl)) ":"
          ;; Line
          (group (+ digit))
          ;; No more content
          (or eol (not (any ":")))))

    (-let* ((str "Error: src/components/ColorList.js:22")
            ((whole file line) (s-match config-web--flow-error-rx str)))
      (cl-assert (equal whole str))
      (cl-assert (equal file "src/components/ColorList.js"))
      (cl-assert (equal line "22")))

    (setf (alist-get 'flow compilation-error-regexp-alist-alist)
          (list config-web--flow-error-rx 1 2))
    (add-to-list 'compilation-error-regexp-alist 'flow)))

(use-package js-refactor-commands
  :commands (js-refactor-commands-organize-imports
             js-refactor-commands-group-and-sort-imports
             js-refactor-commands-expand-comma-bindings
             js-refactor-commands-align-object-literal-values
             js-refactor-commands-toggle-sealed-object-type))

(use-package js-test-commands
  :commands (js-test-commands-test-this-file-dwim))


;; Node

(use-package nvm
  :straight t
  :hook (web-mode . config-web-maybe-use-nvm)
  :functions (nvm-use-for-buffer)
  :custom ((nvm-dir "~/.config/nvm"))
  :preface
  (defun config-web-maybe-use-nvm ()
    (when (locate-dominating-file default-directory ".nvmrc")
      (nvm-use-for-buffer)
      t)))

(provide 'config-web-mode)

;;; config-web-mode.el ends here
