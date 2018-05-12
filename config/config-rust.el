;;; config-rust.el --- Configuration for rust-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 's)

(autoload 'yas-funcs-bolp "cb-yas-elisp")
(autoload 'evil-define-key "evil")

(use-package rust-mode
  :straight t
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
  (progn
    ;; Enable backtraces in rust programs run from Emacs.
    (setenv "RUST_BACKTRACE" "1")

    (evil-define-key 'normal rust-mode-map (kbd "J") #'cb-rust-join-line)))

(autoload 'rust-mode-map "rust-mode" nil nil 'keymap)

(use-package company
  :commands (company-indent-or-complete-common)
  :config
  (with-eval-after-load 'rust-mode
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (evil-define-key 'insert rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (evil-define-key 'normal rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)))

(use-package flycheck-rust
  :straight t
  :commands (flycheck-rust-setup)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package which-key
  :config
  (let ((match-suffix (rx bos "cargo-process-" (group (+ nonl)))))
    (push `((nil . ,match-suffix) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package racer
  :straight t
  :commands (racer-find-definition racer-mode racer-describe)
  :init
  (progn
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'rust-mode-hook #'racer-mode))
  :config
  (progn
    (evil-set-initial-state 'racer-help-mode 'motion)

    ;; Teach compile.el about sources installed via rustup.
    (let ((base (file-name-directory racer-rust-src-path)))
      (add-to-list 'compilation-search-path base t))

    (with-eval-after-load 'rust-mode
      (evil-define-key 'normal rust-mode-map (kbd "K") #'racer-describe)
      (evil-define-key 'normal rust-mode-map (kbd "M-.") #'racer-find-definition))))

(use-package toml-mode
  :straight t
  :mode (("\\.toml\\'" . toml-mode)
         ("\\.Cargo\\.lock\\'" . toml-mode)
         ("\\.cargo/config\\'" . toml-mode)))

(use-package cargo
  :straight t
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
   cargo-process--project-root
   cargo-process-test)
  :init
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'rust-mode
      "." #'cargo-process-repeat
      "C" #'cargo-process-clean
      "X" #'cargo-process-run-example
      "c" #'cargo-process-build
      "d" #'cargo-process-doc
      "e" #'cargo-process-bench
      "f" #'cargo-process-current-test
      "i" #'cargo-process-init
      "n" #'cargo-process-new
      "o" #'cargo-process-current-file-tests
      "s" #'cargo-process-search
      "t" #'cargo-process-test
      "u" #'cargo-process-update
      "x" #'cargo-process-run))
  :preface
  (defun cb-rust--run-in-project-root (f &rest args)
    (let ((default-directory (or (cargo-process--project-root) default-directory)))
      (apply f args)))
  :config
  (progn
    ;; Enable backtraces in Cargo processes started by Emacs.
    (setenv "RUST_BACKTRACE" "1")

    (advice-add 'cargo-process-run :around #'cb-rust--run-in-project-root)))

(use-package rust-faces
  :after rust-mode)

;; Rust backtraces sometimes contain absolute paths from travis builds. Rewrite
;; these to paths relative to the rustup sources directory.

(use-package compile
  :preface
  (progn
    (defun cb-rust--rewrite-compilation-buffer (&optional buf &rest _)
      (with-current-buffer (or buf (current-buffer))
        (save-excursion
          (goto-char (or compilation-filter-start (point-min)))
          (let ((inhibit-read-only t)
                (bad-path "/Users/travis/build/rust-lang/rust/"))
            (while (search-forward-regexp (rx-to-string `(or "" ,bad-path)) nil t)
              (replace-match "" t t)))))))
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'compilation-filter-hook #'cb-rust--rewrite-compilation-buffer)
    (add-to-list 'compilation-finish-functions #'cb-rust--rewrite-compilation-buffer)))

(provide 'config-rust)

;;; config-rust.el ends here
