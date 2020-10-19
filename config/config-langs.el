;;; config-langs.el --- Configuration for simple language packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'general)
  (require 'use-package))

(require 'major-mode-hydra)
(require 'hooks)
(require 'paths)

(autoload 'display-buffer-fullframe "display-buffer-fullframe")
(autoload 'indent-buffer "config-editing")



(use-package apiwrap)

(use-package poporg
  :commands (poporg-dwim))

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :preface
  (defun config-langs--suppress-final-newline ()
    (setq-local require-final-newline nil))
  :config (add-hook 'csv-mode-hook #'config-langs--suppress-final-newline))

(use-package make-mode
  :preface
  (defun config-langs--set-up-makefile-mode ()
    (setq-local tab-width 8)
    (setq-local indent-tabs-mode t))
  :init
  (add-hook 'makefile-mode-hook #'config-langs--set-up-makefile-mode))

(use-package clojure-mode
  :mode ("\\.clj[sd]?\\'" . clojure-mode))

(use-package groovy-mode
  :mode (("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode)
         ("Jenkinsfile\\'" . groovy-mode))
  :interpreter ("groovy" . groovy-mode))

(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         (("\\.gv\\'" . graphviz-dot-mode)))
  :general (:keymaps 'graphviz-dot-mode-map "M-q" #'graphviz-dot-indent-graph)
  :init (general-unbind :keymaps 'graphviz-dot-mode-map "{" "}"))

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package terraform-mode
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode)
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package hcl-mode
  :mode
  (("\\.hcl\\'" . hcl-mode)
   ("\\.nomad\\'" . hcl-mode))
  :preface
  (progn
    (defun config-langs--format-hcl ()
      (when (executable-find "terragrunt")
        (call-process "terragrunt" nil nil nil "hclfmt")))

    (defun config-langs--set-up-hcl-mode ()
      (add-hook 'after-save-hook #'config-langs--format-hcl nil t)))

  :hook (hcl-mode . config-langs--set-up-hcl-mode))

(use-package yaml-mode
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)
  :general
  (:states '(normal insert) :keymaps 'yaml-mode-map
   [backtab] 'yaml-indent-line)
  :preface
  (defun config-langs--disable-autofill ()
    (auto-fill-mode -1))
  :config
  (add-hook 'yaml-mode-hook #'config-langs--disable-autofill))

(use-package json-mode
  :commands (json-mode)
  :mode ("\\.json\\'" . json-mode)
  :custom
  ((json-reformat:indent-width 2)))

(use-package highlight-indent-guides
  :hook ((python-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode))
  :preface
  (defun config-langs--update-ident-guides (&rest _)
    (highlight-indent-guides-auto-set-faces))
  :config
  (add-hook 'after-theme-change-functions #'config-langs--update-ident-guides))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (general-setq lua-indent-level 2))

(use-package lsp-lua-emmy
  :after lua-mode
  :custom
  ((lsp-lua-emmy-jar-path (getenv "NIX_EMACS_EMMY_LUA_JAR"))
   (lsp-lua-emmy-java (expand-file-name "bin/java" (getenv "JAVA_HOME")))))

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :general (:states '(motion normal) :keymaps 'pdf-view-mode-map
            "n" #'pdf-view-next-page
            "N" #'pdf-view-previous-page
            "p" #'pdf-view-previous-page)
  :preface
  (progn
    (defun config-langs--mightnight-mode-for-theme (bg-style)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (derived-mode-p 'pdf-view-mode)
            (pdf-view-midnight-minor-mode (if (equal 'dark bg-style) +1 -1))))))

    (defun config-langs--maybe-pdf-midnight-mode ()
      (when (equal 'dark (frame-parameter nil 'background-mode))
        (pdf-view-midnight-minor-mode +1))))

  :custom
  ((pdf-view-display-size 'fit-page))
  :init
  (progn
    (use-package pdf-history
      :commands (pdf-history-minor-mode))
    (use-package pdf-occur
      :commands (pdf-occur-global-minor-mode)))
  :config
  (progn
    (require 'pdf-annot)
    (require 'pdf-sync)
    (require 'pdf-links)
    (require 'pdf-outline)
    (require 'pdf-history)
    (require 'pdf-cache)
    (require 'pdf-view)

    ;; Redefine a few macros as functions to work around byte compilation errors.
    (defun pdf-view-current-page (&optional window)
      (image-mode-window-get 'page window))

    (defun pdf-view-current-overlay (&optional window)
      (image-mode-window-get 'overlay window))

    (pdf-tools-install)
    (add-hook 'after-theme-change-functions #'config-langs--mightnight-mode-for-theme)
    (add-hook 'pdf-view-mode-hook #'config-langs--maybe-pdf-midnight-mode)))

(use-package graphql-mode
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package pass
  :commands (pass)
  :general
  (:states '(normal) :keymaps 'pass-view-mode-map "q" #'kill-this-buffer)
  (:states '(normal) :keymaps 'pass-mode-map
   "u" #'pass-copy-username
   "U" #'pass-copy-url
   "J" #'pass-goto-entry
   "y" #'pass-copy-password
   "f" #'pass-copy-field)
  :custom
  ((password-store-password-length 50))
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Password-Store*" eos)
                 (display-buffer-reuse-window
                  display-buffer-fullframe)
                 (reusable-frames . visible))))

;; `format-all' applies formatting commands on save, based on major-mode.
(use-package format-all
  :hook (prog-mode . format-all-mode)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*format-all-errors*" eos)
                 (allow-no-window . t))))

(provide 'config-langs)

;;; config-langs.el ends here
