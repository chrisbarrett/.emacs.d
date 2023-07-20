;;; cb-langs.el --- Configuration for simple language packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)

(use-package simple
  :hook
  (text-mode . visual-line-mode))

(use-package conf-mode :mode (rx "." (or "env" "kll" "dockerignore") eos))

(use-package graphql-mode :ensure t :mode (rx "." (or "gql" "graphql") eos))

(use-package terraform-mode :ensure t :mode (rx ".tf" (? "vars") eos))

(use-package hcl-mode :ensure t :mode (rx "." (or "hcl" "nomad") eos))

(use-package swift-mode :ensure t :mode (rx ".swift" eos))

(use-package gnuplot :ensure t)

(use-package plantuml-mode :ensure t :mode (rx "." (or "plantuml" "pum" "plu") eos))

(use-package hexl
  :general
  (:states 'motion :keymaps 'hexl-mode-map
   "]]" #'hexl-end-of-1k-page
   "[[" #'hexl-beginning-of-1k-page
   "h" #'hexl-backward-char
   "l" #'hexl-forward-char
   "j" #'hexl-next-line
   "k" #'hexl-previous-line
   "$" #'hexl-end-of-line
   "^" #'hexl-beginning-of-line
   "0" #'hexl-beginning-of-line))

(use-package sh-script :mode ((rx "/.envrc") . bash-ts-mode)
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  :init
  ;; `bash-ts-mode' falls back to sh-mode, so we don't want to use
  ;; major-mode-remap-alist to set it.
  (setq auto-mode-alist
        (seq-map (pcase-lambda (`(,pat . ,mode))
                   (cons pat (if (equal mode 'sh-mode)
                                 'bash-ts-mode
                               mode)))
                 auto-mode-alist)))

(use-package css-mode
  :custom
  (css-indent-offset 2))

(use-package nix-mode :ensure t :mode (rx ".nix" eos))

(use-package format-all :ensure t
  :config
  ;; KLUDGE: Dynamically eval to avoid macroexpansion error
  (eval
   '(define-format-all-formatter terragrunt-fmt
      (:executable "terragrunt")
      (:install)
      (:languages "Terragrunt")
      (:features)
      (:format (format-all--buffer-easy executable "fmt" "-no-color" "-"))))
  (add-to-list 'format-all-default-formatters 'terragrunt-fmt))

(use-package csharp-mode :mode ((rx ".cs" eos) . csharp-ts-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.csproj$" . nxml-mode))
  (add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode)))

(use-package python :mode ((rx ".py" (? (any "iw")) eos) . python-ts-mode)
  :config
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(use-package dockerfile-ts-mode :mode ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'")
  :config
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode)))

(use-package yaml-ts-mode :mode ((rx "." (or "yaml" "yml") eos) (rx "/yarn.lock" eos))
  :hook
  (yaml-ts-mode-hook . (lambda () (auto-fill-mode -1)))
  :config
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

(use-package json-ts-mode :mode (rx ".json" eos)
  :config
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))

(use-package highlight-indent-guides :ensure t
  :hook ((python-ts-mode yaml-ts-mode json-ts-mode markdown-mode) . highlight-indent-guides-mode))

(use-package oil :mode ((rx ".oil" eos) . oil-mode)
  :magic
  ("#!/.*?/env oil" . oil-mode)
  ("#!/.*?/oil" . oil-mode))

(use-package proof-general :ensure t
  :custom
  (proof-splash-enable nil))

(use-package csv-mode :ensure t :mode (rx ".csv" eos))

(use-package iscroll :ensure t :hook (text-mode . iscroll-mode)
  :general
  (:keymaps 'iscroll-mode-map :states 'normal
   "j" 'iscroll-forward-line
   "k" 'iscroll-previous-line))

(use-package dumb-jump :ensure t
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "graphql" :ext "graphql" :agtype nil :rgtype nil))
  (add-to-list 'dumb-jump-language-file-exts '(:language "graphql" :ext "gql" :agtype nil :rgtype nil))
  (add-to-list 'dumb-jump-find-rules
               '(:type "type" :supports ("ag" "grep" "rg") :language "graphql"
                 :regex "(input|type|union)\\s+JJJ\\b"))
  (add-to-list 'dumb-jump-find-rules
               '(:type "enum"
                 :supports ("ag" "grep" "rg") :language "graphql"
                 :regex "enum\\s+JJJ\\b"))
  (add-to-list 'dumb-jump-find-rules
               '(:type "scalar"
                 :supports ("ag" "grep" "rg") :language "graphql"
                 :regex "scalar\\s+JJJ\\b")))

(use-package origami :ensure t :hook (yaml-ts-mode . origami-mode)
  :general
  (:states 'normal :keymaps 'origami-mode-map
   "TAB" 'origami-recursively-toggle-node
   "S-<tab>" 'origami-toggle-all-nodes))

(use-package edit-indirect :ensure t)

(use-package poporg :ensure t)

(provide 'cb-langs)

;;; cb-langs.el ends here
