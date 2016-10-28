;;; cb-rust.el --- Configuration for rust-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 's)

(autoload 'cb-yas/bol? "cb-yas-elisp")
(autoload 'evil-define-key "evil-core")

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :preface
  (progn
    (autoload 'thing-at-point-looking-at "thingatpt")
    (autoload 'evil-join "evil-commands")

    (setq rust-format-on-save (executable-find "rustfmt"))

    (defun cb-rust-join-line ()
      "Join lines, deleting intermediate spaces for chained function calls."
      (interactive)
      (call-interactively #'evil-join)
      (when (thing-at-point-looking-at (rx (not space) (* space) "."))
        (delete-horizontal-space))))

  :config
  (evil-define-key 'normal rust-mode-map (kbd "J") #'cb-rust-join-line))

(use-package company
  :defer t
  :commands (company-indent-or-complete-common)
  :config
  (progn
    (with-eval-after-load 'rust-mode
      (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
      (evil-define-key 'insert rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
      (evil-define-key 'normal rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

    (add-hook 'rust-mode-hook (lambda ()
                                (setq company-minimum-prefix-length 2)))))

(use-package flycheck-rust
  :commands (flycheck-rust-setup)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(use-package racer
  :defer t
  :commands (racer-find-definition racer-mode)
  :init
  (progn
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'rust-mode-hook #'racer-mode))
  :config
  (progn
    (unless (getenv "RUST_SRC_PATH")
      (setenv "RUST_SRC_PATH" racer-rust-src-path))

    (with-eval-after-load 'rust-mode
      (evil-define-key 'normal rust-mode-map (kbd "M-.") #'racer-find-definition))))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)
         ("\\.Cargo\\.lock\\'" . toml-mode)
         ("\\.cargo/config\\'" . toml-mode)))

(use-package cargo
  :defer t
  :commands
  (cargo-process-repeat
   cargo-process-clean
   cargo-process-run-example
   cargo-process-build
   cargo-process-doc
   cargo-process-bench
   cargo-process-current-test
   cargo-process-fmt
   cargo-process-init
   cargo-process-new
   cargo-process-current-file-tests
   cargo-process-search
   cargo-process-update
   cargo-process-run
   cargo-process-test)
  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'rust-mode "mc" "cargo")
    (spacemacs-keys-set-leader-keys-for-major-mode 'rust-mode
      "c." #'cargo-process-repeat
      "cC" #'cargo-process-clean
      "cX" #'cargo-process-run-example
      "cc" #'cargo-process-build
      "cd" #'cargo-process-doc
      "ce" #'cargo-process-bench
      "cf" #'cargo-process-current-test
      "cf" #'cargo-process-fmt
      "ci" #'cargo-process-init
      "cn" #'cargo-process-new
      "co" #'cargo-process-current-file-tests
      "cs" #'cargo-process-search
      "cu" #'cargo-process-update
      "cx" #'cargo-process-run
      "t"  #'cargo-process-test)))


;; Snippet Utilities

(defun cb-rust-bol-or-after-accessibility-modifier? ()
  "Predicate for snippets"
  (save-excursion
    (save-restriction
      ;; Move past access modifier.
      (goto-char (line-beginning-position))
      (search-forward-regexp (rx bow "pub" eow (* space)) (line-end-position) t)
      (narrow-to-region (point) (line-end-position))
      (cb-yas/bol?))))

(defun cb-rust-previous-struct-def ()
  "Search backward for the name of the last struct defined in this file."
  (save-match-data
    (if (search-backward-regexp (rx (or "enum" "struct") (+ space)
                                    (group (+ (not (any ";" "(" "{")))))
                                nil t)
        (s-trim (match-string 1))
      "Name")))


(provide 'cb-rust)

;;; cb-rust.el ends here
