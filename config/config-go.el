;;; config-go.el --- Configuration for golang.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 's)
(require 'spacemacs-keys)

(autoload 'evil-define-key "evil")
(autoload 'projectile-project-p "projectile")

(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode)

  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "me" "playground")
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mg" "goto")
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mh" "help")
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mi" "imports")

    (spacemacs-keys-set-leader-keys-for-major-mode 'go-mode
      "hh" 'godoc-at-point
      "ig" 'go-goto-imports
      "ia" 'go-import-add
      "ir" 'go-remove-unused-imports
      "eb" 'go-play-buffer
      "er" 'go-play-region
      "ed" 'go-download-play
      "ga" 'ff-find-other-file
      "gc" 'go-coverage))

  :preface
  (progn
    (defun cb-go-lookup-go-root ()
      (-let* ((default-directory (or (projectile-project-p) default-directory))
              (output (s-lines (s-trim (shell-command-to-string "go env"))))
              ((&alist "GOROOT" go-root)
               (--map (-let* (((var val) (s-split "=" it))
                              ((_ val) (s-match (rx "\"" (group (*? nonl)) "\"") val)))
                        (cons var val))
                      output)))
        go-root))

    (defun cb-go--set-local-vars ()
      (setq-local tab-width 4)
      (setq-local indent-tabs-mode t)
      (with-no-warnings
        (setq-local evil-shift-width 4))
      (unless (getenv "GOROOT")
        (setenv "GOROOT" (cb-go-lookup-go-root)))))

  :config
  (progn
    (setq gofmt-show-errors nil)
    (evil-define-key 'normal go-mode-map (kbd "K") #'godoc-at-point)

    (evil-define-key 'normal go-mode-map (kbd "M-.") #'godef-jump)
    (evil-define-key 'insert go-mode-map (kbd "M-.") #'godef-jump)

    (add-hook 'go-mode-hook #'cb-go--set-local-vars)
    (add-hook 'before-save-hook #'gofmt-before-save))

  :functions (gofmt-before-save godoc-at-point))

(use-package company-go
  :straight t
  :after go-mode

  :preface
  (progn
    (autoload 'company-mode "company")

    (defun cb-go-company-setup ()
      (with-no-warnings
        (setq-local company-backends '(company-go)))
      (company-mode)))

  :config
  (progn
    (with-no-warnings
      (setq company-go-show-annotation t))
    (add-hook 'go-mode-hook #'cb-go-company-setup)))

(use-package go-eldoc
  :straight t
  :after go-mode
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-run-cmds
  :after go-mode
  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mt" "test")
    (spacemacs-keys-declare-prefix-for-mode 'go-mode "mx" "execute")
    (spacemacs-keys-set-leader-keys-for-major-mode
      'go-mode
      "tt" 'go-run-cmds/run-test-current-function
      "ts" 'go-run-cmds/run-test-current-suite
      "tp" 'go-run-cmds/run-package-tests
      "tP" 'go-run-cmds/run-package-tests-nested
      "x" 'go-run-cmds/run-main))
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*go " (or "test" "run") "*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (slot            . 0)
                 (window-height   . 0.2))))

(provide 'config-go)

;;; config-go.el ends here
