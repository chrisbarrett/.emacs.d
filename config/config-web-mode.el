;;; config-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'config-etags)
  (require 'use-package))

(require 'cb-major-mode-hydra)
(require 'f)
(require 's)
(require 'subr-x)

(autoload 'flycheck-add-mode "flycheck")
(autoload 'projectile-project-p "projectile")



(cb-major-mode-hydra-define web-js-mode
  "Refactor"
  (("ro" js-refactor-commands-organize-imports "organise imports")
   ("ra" js-refactor-commands-align-object-literal-values "align object values")
   ("re" js-refactor-commands-expand-comma-bindings "expand comma bindings")
   ("rs" js-refactor-commands-toggle-sealed-object-type "toggle sealed type"))

  "Add Breakpoints"
  (("bb" indium-add-breakpoint "add")
   ("bc" indium-add-conditional-breakpoint "add conditional")
   ("bl" indium-list-breakpoints "list")
   ("ba" indium-activate-breakpoints "activate"))

  "Modify Breakpoints"
  (("be" indium-edit-breakpoint-condition "edit condition")
   ("bx" indium-remove-breakpoint "remove")
   ("bX" indium-remove-all-breakpoints-from-buffer "remove all")
   ("bd" indium-deactivate-breakpoints "deactivate"))

  "Run"
  (("xn" indium-run-node "node")
   ("xc" indium-run-chrome "chrome")))



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

  :init
  (setq web-mode-extra-keywords '(("javascript" . ("type"))))

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
  :hook (web-mode . rainbow-mode))

(use-package web-mode-submodes
  :defer t
  :mode (("\\.json\\'" . web-json-mode)
         ("\\.eslintrc\\'" . web-json-mode)
         ("\\.babelrc\\'" . web-json-mode)
         ("\\.es6\\'"  . web-js-mode)
         ("\\.tsx?\\'"  . web-ts-mode)
         ("\\.jsx?\\'" . web-js-mode)
         ("\\.css\\'"  . web-css-mode)
         ("\\.mustache\\'"  . web-mustache-mode)
         ("\\.scss\\'"  . web-css-mode)
         ("\\.html\\'" . web-html-mode)
         ("\\.avsc\\'" . avro-mode)
         ("\\.avro\\'" . avro-mode))
  :preface
  (defun config-web--enable-readonly-mode-in-node-modules ()
    (when (and (buffer-file-name)
               (string-match-p (rx "/node_modules/") (buffer-file-name)))
      (read-only-mode +1)))
  :init
  (add-hook 'find-file-hook #'config-web--enable-readonly-mode-in-node-modules))

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
    (flycheck-add-mode 'html-tidy 'web-html-mode)))

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
  (with-eval-after-load 'web-mode
    (define-key web-mode-map (kbd "<C-return>") #'emmet-expand-line))

  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (add-hook 'web-js-mode-hook #'config-web--set-jsx-classname-on)))

(use-package flycheck-flow
  :straight t
  :after (:and web-mode-submodes flycheck)
  :config
  (progn
    (flycheck-add-mode 'javascript-flow 'web-js-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))

(use-package flow
  :commands (flow-insert-flow-annotation)
  :general (:keymaps 'web-js-mode-map "C-c C-t" #'flow-type-at))

(use-package tern
  :straight t
  :disabled t
  :commands (tern-mode)
  :hook (web-js-mode . config-web--maybe-enable-tern)
  :general
  (:states 'normal :keymaps 'tern-mode-keymap
   "K" 'tern-get-docs
   "gd"  'tern-find-definition
   "M-." 'tern-find-definition
   "M-," 'tern-pop-find-definition)
  :preface
  (progn
    (autoload 'flycheck-overlay-errors-at "flycheck")

    (defun config-web--maybe-enable-tern ()
      (unless config-etags-in-query-replace-session-p
        (tern-mode +1)))

    (defun config-web--flycheck-errors-at-point-p ()
      (when (bound-and-true-p flycheck-mode)
        (flycheck-overlay-errors-at (point))))

    (defun config-web--maybe-suppress-tern-hints (f &rest args)
      (unless (config-web--flycheck-errors-at-point-p)
        (apply f args))))

  :config
  (progn
    (setq tern-command (add-to-list 'tern-command "--no-port-file" t))

    (unless (getenv "NODE_PATH")
      (setenv "NODE_PATH" "/usr/local/lib/node_modules"))

    (advice-add 'tern-show-argument-hints :around #'config-web--maybe-suppress-tern-hints)))

(use-package company-tern
  :straight t
  :after (:and web-mode-submodes tern)
  :config
  (progn
    (setq company-tern-meta-as-single-line t)
    (setq company-tern-property-marker " <p>")
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-tern))))

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
  :commands (prettier-js-mode prettier-js)
  :after web-mode-submodes
  :preface
  (progn
    (defvar prettier-js-inhibited-for-project nil)

    (put 'prettier-js-inhibited-for-project 'safe-local-variable 'symbolp)

    (defun config-web--child-file-of-node-modules-p ()
      (and (buffer-file-name) (string-match-p "/node_modules/" (buffer-file-name))))

    (defun config-web--maybe-enable-prettier ()
      (unless prettier-js-inhibited-for-project
        (when (and (derived-mode-p 'web-js-mode 'web-ts-mode)
                   (not (config-web--child-file-of-node-modules-p)))
          (prettier-js-mode +1))))

    (define-globalized-minor-mode prettier-js-global-mode
      prettier-js-mode config-web--maybe-enable-prettier))

  :config
  (prettier-js-global-mode +1))

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

;; Node

(use-package indium
  :straight t
  :commands (indium-run-node)
  :hook
  ((web-js-mode . indium-interaction-mode)
   (js-mode . indium-interaction-mode))

  :preface
  (defun node-repl (arg)
    "Run node. With prefix ARG, prompt for the command to run."
    (interactive "P")
    (if arg
        (call-interactively #'indium-run-node)
      (indium-run-node "node")))
  :init
  (defalias 'run-node #'node-repl)
  :general (:keymaps 'web-js-mode-map "C-c C-l" #'indium-eval-buffer)
  :general
  (:states 'motion :keymaps indium-inspector-mode-map
   "^" #'indium-inspector-pop
   "r" #'indium-inspector-refresh))

(use-package nvm
  :straight t
  :hook (web-mode . config-web-maybe-use-nvm)
  :functions (nvm-use-for-buffer)
  :preface
  (defun config-web-maybe-use-nvm ()
    (when (locate-dominating-file default-directory ".nvmrc")
      (nvm-use-for-buffer)
      t)))

(provide 'config-web-mode)

;;; config-web-mode.el ends here
