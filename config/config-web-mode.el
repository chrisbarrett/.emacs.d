;;; config-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'config-etags)
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)
(require 's)
(require 'f)

(autoload 'evil-define-key "evil")
(autoload 'evil-set-initial-state "evil")
(autoload 'projectile-project-p "projectile")
(autoload 'flycheck-add-mode "flycheck")

(use-package web-mode
  :straight t
  :defines (web-mode-markup-indent-offset
            web-mode-css-indent-offset)
  :mode (("\\.php\\'" . web-mode))

  :defer t

  :preface
  (progn
    (autoload 'sp-local-pair "smartparens")
    (autoload 'projectile-project-p "projectile")

    (defun cb-web--node-modules-bin-dir ()
      (when-let* ((root (projectile-project-p))
                  (dir (f-join root "node_modules" ".bin")))
        (when (f-dir? dir)
          dir)))

    (defun cb-web--add-node-modules-bin-to-exec-path ()
      (when-let* ((bin (cb-web--node-modules-bin-dir)))
        (setq-local exec-path (cons bin exec-path)))))

  :init
  (setq web-mode-extra-keywords '(("javascript" . ("type"))))

  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)

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

    ;; Run programs out of node_bin

    (add-hook 'web-mode-hook #'cb-web--add-node-modules-bin-to-exec-path)))

(use-package rainbow-mode
  :straight t
  :commands (rainbow-mode)
  :init
  (add-hook 'web-mode-hook #'rainbow-mode))

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
         ("\\.html\\'" . web-html-mode))
  :preface
  (defun cb-web--enable-readonly-mode-in-node-modules ()
    (when (and (buffer-file-name)
               (string-match-p (rx "/node_modules/") (buffer-file-name)))
      (read-only-mode +1)))
  :init
  (add-hook 'find-file-hook #'cb-web--enable-readonly-mode-in-node-modules))

(use-package flycheck
  :defer t
  :commands (flycheck-select-checker)
  :functions (flycheck-add-next-checker flycheck-add-mode)
  :preface
  (defun cb-web--disable-flycheck-for-node-modules ()
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

    (add-hook 'web-js-mode-hook #'cb-web--disable-flycheck-for-node-modules)
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
  :defines (emmet-expand-jsx-className?)
  :commands (emmet-mode emmet-expand-yas)
  :preface
  (progn
    (autoload 'sp-get-enclosing-sexp "smartparens")
    (autoload 'sp-up-sexp "smartparens")
    (autoload 'yas--templates-for-key-at-point "yasnippet")
    (autoload 'yas-expand "yasnippet")

    (defun cb-web--set-jsx-classname-on ()
      (setq-local emmet-expand-jsx-className? t))

    (defun cb-web--maybe-emmet-mode ()
      (cond
       ((derived-mode-p 'web-html-mode 'html-mode 'nxml-mode)
        (emmet-mode +1))

       ((equal web-mode-content-type "html")
        (emmet-mode +1))))

    (defun cb-web--emmet-move-out-of-squares ()
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

    (defun cb-web-expand-snippet-then-emmet ()
      (interactive)
      (if (yas--templates-for-key-at-point)
          (call-interactively #'yas-expand)
        (cb-web--emmet-move-out-of-squares)
        (emmet-expand-yas))))

  :init
  (progn
    (evil-leader/set-key-for-mode 'web-js-mode (kbd "e") #'emmet-mode)
    (add-hook 'web-mode-hook #'cb-web--maybe-emmet-mode))
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (define-key emmet-mode-keymap (kbd "TAB") #'cb-web-expand-snippet-then-emmet)
    (add-hook 'web-js-mode-hook #'cb-web--set-jsx-classname-on)))

(use-package flycheck-flow
  :straight t
  :after flycheck
  :config
  (progn
    (flycheck-add-mode 'javascript-flow 'web-js-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))

(use-package flow
  :after web-mode-submodes
  :commands (flow-insert-flow-annotation
             flow-type-at)
  :init
  (spacemacs-keys-set-leader-keys-for-major-mode 'web-js-mode
    "if" #'flow-insert-flow-annotation)
  :bind (:map web-js-mode-map
              ("C-c C-t" . flow-type-at)))

(use-package js-autoinsert
  :after autoinsert
  :config
  (add-to-list 'auto-insert-alist
               '((web-js-mode . "JavaScript") . js-autoinsert-template-string)))

(use-package tern
  :straight t
  :disabled t
  :commands (tern-mode)
  :preface
  (progn
    (autoload 'flycheck-overlay-errors-at "flycheck")

    (defun cb-web--maybe-enable-tern ()
      (unless config-etags-in-query-replace-session-p
        (tern-mode +1)))

    (defun cb-web--flycheck-errors-at-point-p ()
      (when (bound-and-true-p flycheck-mode)
        (flycheck-overlay-errors-at (point))))

    (defun cb-web--maybe-suppress-tern-hints (f &rest args)
      (unless (cb-web--flycheck-errors-at-point-p)
        (apply f args))))

  :init
  (add-hook 'web-js-mode-hook #'cb-web--maybe-enable-tern)

  :config
  (progn
    (setq tern-command (add-to-list 'tern-command "--no-port-file" t))

    (unless (getenv "NODE_PATH")
      (setenv "NODE_PATH" "/usr/local/lib/node_modules"))

    (evil-define-key 'normal tern-mode-keymap
      (kbd "K") 'tern-get-docs
      (kbd "gd")  'tern-find-definition
      (kbd "M-.") 'tern-find-definition
      (kbd "M-,") 'tern-pop-find-definition)

    (advice-add 'tern-show-argument-hints :around #'cb-web--maybe-suppress-tern-hints)))

(use-package company-tern
  :straight t
  :after (web-mode-submodes tern)
  :config
  (progn
    (setq company-tern-meta-as-single-line t)
    (setq company-tern-property-marker " <p>")

    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-tern))))

(use-package web-beautify
  :straight t
  :commands (web-beautify-js web-beautify-html web-beautify-css)
  :init
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'web-json-mode "=" #'web-beautify-js)
    (spacemacs-keys-set-leader-keys-for-major-mode 'web-html-mode  "=" #'web-beautify-html)
    (spacemacs-keys-set-leader-keys-for-major-mode 'web-css-mode "=" #'web-beautify-css)))

(use-package stylus-mode
  :straight t
  :mode ("\\.styl\\'" . stylus-mode)
  :preface
  (defun cb-web--set-stylus-vars ()
    (setq-local tab-width 2))
  :config
  (add-hook 'stylus-mode-hook #'cb-web--set-stylus-vars))

(use-package aggressive-indent
  :defer t
  :preface
  (defun cb-web--in-flow-strict-object-type? ()
    (when (derived-mode-p 'web-js-mode)
      (-let [(depth start) (syntax-ppss)]
        (and (cl-plusp depth)
             (eq (char-after start) ?{)
             (eq (char-after (1+ start)) ?|)))))
  :config
  (progn
    (add-to-list 'aggressive-indent-dont-indent-if '(cb-web--in-flow-strict-object-type?))
    (add-hook 'aggressive-indent-stop-here-hook #'cb-web--in-flow-strict-object-type?)))

(use-package which-key
  :config
  (let* ((boring-prefixes '("indium" "flow" "tide" "js-refactor-commands"))
         (match-prefix (rx-to-string `(and bos (or ,@boring-prefixes) "-" (group (+ nonl)))
                                     t)))
    (push `((nil . ,match-prefix) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package prettier-js
  :straight t
  :commands (prettier-js-mode prettier-js)
  :after web-mode-submodes
  :preface
  (progn
    (defvar prettier-js-inhibited-for-project nil)

    (put 'prettier-js-inhibited-for-project 'safe-local-variable 'symbolp)

    (defun cb-web--child-file-of-node-modules-p ()
      (and (buffer-file-name) (string-match-p "/node_modules/" (buffer-file-name))))

    (defun cb-web--maybe-enable-prettier ()
      (unless prettier-js-inhibited-for-project
        (when (and (derived-mode-p 'web-js-mode 'web-ts-mode)
                   (not (cb-web--child-file-of-node-modules-p)))
          (prettier-js-mode +1))))

    (define-globalized-minor-mode prettier-js-global-mode
      prettier-js-mode cb-web--maybe-enable-prettier))

  :config
  (prettier-js-global-mode +1))

(use-package compile
  :defer t
  :config
  (progn
    (defconst cb-web--flow-error-rx
      (rx bol "Error:" (+ space)
          ;; Filename
          (group (+? nonl)) ":"
          ;; Line
          (group (+ digit))))

    (-let* ((str "Error: src/components/ColorList.js:22")
            ((whole file line) (s-match cb-web--flow-error-rx str)))
      (cl-assert (equal whole str))
      (cl-assert (equal file "src/components/ColorList.js"))
      (cl-assert (equal line "22")))

    (setf (alist-get 'flow compilation-error-regexp-alist-alist)
          (list cb-web--flow-error-rx 1 2))
    (add-to-list 'compilation-error-regexp-alist 'flow)))

(use-package js-refactor-commands
  :commands (js-refactor-commands-organize-imports
             js-refactor-commands-group-and-sort-imports
             js-refactor-commands-expand-comma-bindings
             js-refactor-commands-align-object-literal-values
             js-refactor-commands-toggle-sealed-object-type)
  :init
  (dolist (mode '(web-js-mode web-ts-mode))
    (spacemacs-keys-declare-prefix-for-mode mode "mr" "refactor")
    (spacemacs-keys-set-leader-keys-for-major-mode mode
      "ro" #'js-refactor-commands-organize-imports
      "ra" #'js-refactor-commands-align-object-literal-values
      "re" #'js-refactor-commands-expand-comma-bindings
      "rs" #'js-refactor-commands-toggle-sealed-object-type)))

;; Node

(use-package indium
  :straight t
  :commands (indium-interaction-mode
             indium-run-node)
  :preface
  (defun node-repl (arg)
    "Run node. With prefix ARG, prompt for the command to run."
    (interactive "P")
    (if arg
        (call-interactively #'indium-run-node)
      (indium-run-node "node")))
  :init
  (progn
    (defalias 'run-node #'node-repl)

    (add-hook 'web-js-mode-hook #'indium-interaction-mode)
    (add-hook 'js-mode-hook #'indium-interaction-mode)

    (dolist (mode '(web-js-mode js-mode))
      (spacemacs-keys-declare-prefix-for-mode mode "md" "debugger")
      (spacemacs-keys-declare-prefix-for-mode mode "mi" "flow")
      (spacemacs-keys-declare-prefix-for-mode mode "mx" "run")
      (spacemacs-keys-set-leader-keys-for-major-mode mode
        "d b" 'indium-add-breakpoint
        "d B" 'indium-add-conditional-breakpoint
        "d x" 'indium-remove-breakpoint
        "d X" 'indium-remove-all-breakpoints-from-buffer
        "d e" 'indium-edit-breakpoint-condition
        "d l" 'indium-list-breakpoints
        "d d" 'indium-deactivate-breakpoints
        "d a" 'indium-activate-breakpoints
        "x n" 'indium-run-node
        "x c" 'indium-run-chrome)))

  :config
  (progn
    (evil-set-initial-state 'indium-inspector-mode 'motion)
    (evil-set-initial-state 'indium-repl-mode 'insert)
    (evil-define-key 'motion indium-inspector-mode-map (kbd "^") 'indium-inspector-pop)
    (evil-define-key 'motion indium-inspector-mode-map (kbd "r") 'indium-inspector-refresh)
    (with-eval-after-load 'web-mode-submodes
      (define-key web-js-mode-map (kbd "C-c C-l") 'indium-eval-buffer))))

;; Typescript

(with-eval-after-load 'typescript-mode
  (defalias 'typescript-mode #'web-ts-mode))

(use-package tide
  :straight t
  :commands (tide-setup
             tide-restart-server
             tide-references
             tide-documentation-at-point
             tide-rename-symbol
             tide-fix
             tide-refactor
             tide-jump-to-definition
             tide-jump-back
             tide-jsdoc-template)
  :preface
  (defun cb-web--setup-ts-mode ()
    (tide-setup)
    (with-no-warnings
      (setq-local evil-lookup-func #'tide-documentation-at-point)))
  :init
  (progn
    (add-hook 'web-ts-mode-hook #'cb-web--setup-ts-mode)
    (spacemacs-keys-declare-prefix-for-mode 'web-ts-mode "r" "refactor")

    (spacemacs-keys-set-leader-keys-for-major-mode 'web-ts-mode
      "d" #'tide-jsdoc-template
      "x" #'tide-restart-server
      "f" #'tide-references
      "rn" #'tide-rename-symbol
      "rr" #'tide-refactor
      "rf" #'tide-fix)

    (with-eval-after-load 'web-mode-submodes
      (evil-define-key 'normal web-ts-mode-map
        (kbd "K") #'tide-documentation-at-point
        (kbd "M-.") #'tide-jump-to-definition
        (kbd "M-,") #'tide-jump-back)))
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'typescript-tide 'web-ts-mode)
    (flycheck-add-mode 'javascript-tide 'web-js-mode)
    (flycheck-add-mode 'jsx-tide 'web-js-mode)
    (flycheck-add-mode 'tsx-tide 'web-js-mode)))

(use-package ts-comint
  :straight t
  :commands (run-ts
             ts-send-last-sexp
             ts-send-last-sexp-and-go
             ts-send-buffer
             ts-send-buffer-and-go
             ts-load-file-and-go)
  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'web-ts-mode "e" "eval")

    (spacemacs-keys-set-leader-keys-for-major-mode 'web-ts-mode
      "l" #'ts-load-file-and-go
      "eb" #'ts-send-buffer
      "eB" #'ts-send-buffer-and-go
      "es" #'ts-send-last-sexp
      "eS" #'ts-send-last-sexp-and-go)

    (with-eval-after-load 'web-mode-submodes
      (let ((km web-ts-mode-map))
        (define-key km (kbd "C-x C-e") #'ts-send-last-sexp)
        (define-key km (kbd "C-c C-b") #'ts-send-buffer)
        (define-key km (kbd "C-c C-l") #'ts-load-file-and-go)))))

(use-package nvm
  :straight t
  :after web-mode-submodes
  :functions (nvm-use-for-buffer)
  :preface
  (defun cb-web-maybe-use-nvm ()
    (when (locate-dominating-file default-directory ".nvmrc")
      (nvm-use-for-buffer)))
  :config
  (add-hook 'web-mode-hook #'cb-web-maybe-use-nvm))

;; Avro file mode

(autoload 'web-mode "web-mode")

(define-derived-mode avro-mode web-mode "Avro"
  "Derived mode for editing Avro schema files."
  (setq-local web-mode-content-type "json"))

(add-to-list 'auto-mode-alist '("\\.avsc" . avro-mode))

(provide 'config-web-mode)

;;; config-web-mode.el ends here
