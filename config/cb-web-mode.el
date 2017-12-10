;;; cb-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(autoload 'evil-set-initial-state "cb-evil")
(autoload 'evil-define-key "evil-core")
(autoload 'projectile-project-p "projectile")
(autoload 'f-join "f")
(autoload 'f-split "f")

(use-package web-mode
  :defines (web-mode-markup-indent-offset
            web-mode-css-indent-offset)

  :defer t

  :preface
  (autoload 'sp-local-pair "smartparens")

  :init
  (setq web-mode-extra-keywords '(("javascript" . ("type"))))

  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)

    ;; Disable web-mode-reload binding
    (define-key web-mode-map (kbd "C-c C-r") nil)

    ;; Use line comments when commenting in JS.

    (setf (cdr (assoc "javascript" web-mode-comment-formats)) "//")

    ;; Change default indentation behaviour.

    (setf (cdr (assoc "lineup-args" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-concats" web-mode-indentation-params)) nil)
    (setf (cdr (assoc "lineup-calls" web-mode-indentation-params)) nil)

    ;; Treat es6 files as JS files.

    (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
    (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))))

(use-package cb-web-modes
  :defer t
  :mode (("\\.json\\'" . cb-web-json-mode)
         ("\\.eslintrc\\'" . cb-web-json-mode)
         ("\\.babelrc\\'" . cb-web-json-mode)
         ("\\.es6\\'"  . cb-web-js-mode)
         ("\\.tsx?\\'"  . cb-web-typescript-mode)
         ("\\.jsx?\\'" . cb-web-js-mode)
         ("\\.css\\'"  . cb-web-css-mode)
         ("\\.mustache\\'"  . cb-web-mustache-mode)
         ("\\.scss\\'"  . cb-web-css-mode)
         ("\\.html\\'" . cb-web-html-mode))
  :defines (flycheck-html-tidy-executable)
  :config
  (with-eval-after-load 'flycheck
    (let ((tidy-bin "/usr/local/Cellar/tidy-html5/5.2.0/bin/tidy"))
      (when (file-exists-p tidy-bin)
        (setq flycheck-html-tidy-executable tidy-bin)))

    (flycheck-add-mode 'typescript-tslint 'cb-web-typescript-mode)

    (flycheck-add-mode 'javascript-eslint 'cb-web-js-mode)
    (flycheck-add-mode 'javascript-jscs 'cb-web-js-mode)
    (flycheck-add-mode 'javascript-jshint 'cb-web-js-mode)
    (flycheck-add-mode 'javascript-standard 'cb-web-js-mode)

    (flycheck-add-mode 'css-csslint 'cb-web-css-mode)
    (flycheck-add-mode 'json-jsonlint 'cb-web-json-mode)
    (flycheck-add-mode 'html-tidy 'cb-web-html-mode)))

(use-package flycheck
  :defer t
  :commands (flycheck-select-checker)
  :functions (flycheck-add-next-checker flycheck-add-mode)
  :preface

  (defun cb-web--add-node-modules-bin-to-path ()
    "Use binaries from node_modules, where available."
    (when-let (root (projectile-project-p))
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path (f-join root "node_modules" ".bin"))))

  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)

    (add-hook 'cb-web-typescript-mode-hook #'cb-web--add-node-modules-bin-to-path)
    (add-hook 'cb-web-js-mode-hook #'cb-web--add-node-modules-bin-to-path)))

(use-package emmet-mode
  :defer t
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
       ((derived-mode-p 'cb-web-html-mode 'html-mode 'nxml-mode)
        (emmet-mode +1))

       ((equal web-mode-content-type "html")
        (emmet-mode +1))

       ((and (derived-mode-p 'cb-web-js-mode)
             (buffer-file-name)
             (memq "components" (f-split (buffer-file-name))))
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
  (add-hook 'web-mode-hook #'cb-web--maybe-emmet-mode)
  :config
  (progn
    (setq emmet-move-cursor-between-quotes t)
    (define-key emmet-mode-keymap (kbd "TAB") #'cb-web-expand-snippet-then-emmet)
    (add-hook 'cb-web-js-mode-hook #'cb-web--set-jsx-classname-on)))

(use-package flycheck-flow
  :after flycheck
  :config
  (progn
    (flycheck-add-mode 'javascript-flow 'cb-web-js-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)))

(use-package cb-flow
  :after cb-web-modes
  :commands (cb-flow-insert-flow-annotation
             cb-flow-type-at)
  :init
  (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-js-mode
    "if" #'cb-flow-insert-flow-annotation)
  :bind (:map cb-web-js-mode-map
              ("C-c C-t" . cb-flow-type-at)))

(use-package cb-js-autoinsert
  :defer t
  :after autoinsert
  :config
  (add-to-list 'auto-insert-alist
               '((cb-web-js-mode . "JavaScript") . cb-js-autoinsert-template-string))
  :defines (auto-insert-alist))

(use-package cb-html-autoinsert
  :defer t
  :after autoinsert
  :config
  (add-to-list 'auto-insert-alist
               '((cb-web-html-mode . "HTML") . cb-html-autoinsert-template-string))
  :defines (auto-insert-alist))

(use-package tern
  :commands (tern-mode)
  :init
  (add-hook 'cb-web-js-mode-hook #'tern-mode)
  :config
  (progn
    (setq tern-command (add-to-list 'tern-command "--no-port-file" t))

    (unless (getenv "NODE_PATH")
      (setenv "NODE_PATH" "/usr/local/lib/node_modules"))

    (evil-define-key 'normal tern-mode-keymap
      (kbd "K") 'tern-get-docs
      (kbd "M-.") 'tern-find-definition
      (kbd "M-,") 'tern-pop-find-definition)))

(use-package company-tern
  :after cb-web-modes
  :config
  (progn
    (setq company-tern-meta-as-single-line t)
    (setq company-tern-property-marker " <p>")

    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-tern))))

(use-package web-beautify
  :commands (web-beautify-js web-beautify-html web-beautify-css)
  :init
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-js-mode  "=" #'web-beautify-js)
    (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-json-mode "=" #'web-beautify-js)
    (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-html-mode  "=" #'web-beautify-html)
    (spacemacs-keys-set-leader-keys-for-major-mode 'cb-web-css-mode "=" #'web-beautify-css)))

(use-package stylus-mode
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
    (when (derived-mode-p 'cb-web-js-mode)
      (-let [(depth start) (syntax-ppss)]
        (and (plusp depth)
             (eq (char-after start) ?{)
             (eq (char-after (1+ start)) ?|)))))
  :config
  (progn
    (add-to-list 'aggressive-indent-dont-indent-if '(cb-web--in-flow-strict-object-type?))
    (add-hook 'aggressive-indent-stop-here-hook #'cb-web--in-flow-strict-object-type?)))

(use-package indium
  :commands (indium-interaction-mode
             indium-run-node)
  :init
  (progn
    (defalias 'run-node #'indium-run-node)
    (defalias 'node-repl #'indium-run-node)

    (add-hook 'cb-web-js-mode-hook #'indium-interaction-mode)
    (add-hook 'js-mode-hook #'indium-interaction-mode)

    (dolist (mode '(cb-web-js-mode js-mode))
      (spacemacs-keys-declare-prefix-for-mode mode "md" "debugger")
      (spacemacs-keys-declare-prefix-for-mode mode "mi" "flow")
      (spacemacs-keys-declare-prefix-for-mode mode "mr" "run")
      (spacemacs-keys-set-leader-keys-for-major-mode mode
        "d b" 'indium-add-breakpoint
        "d B" 'indium-add-conditional-breakpoint
        "d x" 'indium-remove-breakpoint
        "d X" 'indium-remove-all-breakpoints-from-buffer
        "d e" 'indium-edit-breakpoint-condition
        "d l" 'indium-list-breakpoints
        "d d" 'indium-deactivate-breakpoints
        "d a" 'indium-activate-breakpoints
        "r n" 'indium-run-node
        "r c" 'indium-run-chrome)))

  :config
  (progn
    (evil-set-initial-state 'indium-inspector-mode 'motion)
    (evil-set-initial-state 'indium-repl-mode 'insert)
    (evil-define-key 'motion indium-inspector-mode-map (kbd "^") 'indium-inspector-pop)
    (evil-define-key 'motion indium-inspector-mode-map (kbd "r") 'indium-inspector-refresh)
    (define-key cb-web-js-mode-map (kbd "C-c C-l") 'indium-eval-buffer)))

(use-package which-key
  :config
  (let* ((boring-prefixes '("indium" "cb-flow"))
         (match-prefix (rx-to-string `(and bos (or ,@boring-prefixes) "-" (group (+ nonl)))
                                     t)))
    (push `((nil . ,match-prefix) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package prettier-js
  :commands (prettier-js-mode)
  :init
  (add-hook 'cb-web-js-mode-hook #'prettier-js-mode))


;; Avro file mode

(autoload 'web-mode "web-mode")

(define-derived-mode avro-mode web-mode "Avro"
  "Derived mode for editing Avro schema files."
  (setq-local web-mode-content-type "json"))

(add-to-list 'auto-mode-alist '("\\.avsc" . avro-mode))


(provide 'cb-web-mode)

;;; cb-web-mode.el ends here
