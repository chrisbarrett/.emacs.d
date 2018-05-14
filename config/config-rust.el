;;; config-rust.el --- Configuration for rust-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)
(require 's)

(autoload 'evil-define-key "evil")



(major-mode-hydra-bind rust-mode "Project"
  ("b" cargo-process-build "build")
  ("c" cargo-process-clean "clean")
  ("u" cargo-process-update "update")
  ("d" cargo-process-doc "docs"))

(major-mode-hydra-bind rust-mode "Test"
  ("t" cargo-process-test "all tests")
  ("o" cargo-process-current-file-tests "file tests")
  ("f" cargo-process-current-test "current test")
  ("e" cargo-process-bench "benchmarks"))

(major-mode-hydra-bind rust-mode "Misc"
  ("." cargo-process-repeat "repeat last cargo command")
  ("s" cargo-process-search "search")
  ("x" cargo-process-run "run"))



(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :preface
  (progn
    (autoload 'company-indent-or-complete-common "company")
    (autoload 'thing-at-point-looking-at "thingatpt")
    (autoload 'evil-join "evil-commands")

    (setq rust-format-on-save (executable-find "rustfmt"))

    (defun config-rust-join-line ()
      "Join lines, deleting intermediate spaces for chained function calls."
      (interactive)
      (call-interactively #'evil-join)
      (when (thing-at-point-looking-at (rx (not space) (* space) "."))
        (delete-horizontal-space))))

  :config
  (progn
    ;; Enable backtraces in rust programs run from Emacs.
    (setenv "RUST_BACKTRACE" "1")
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

    (evil-define-key 'insert rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (evil-define-key 'normal rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

    (evil-define-key 'normal rust-mode-map (kbd "J") #'config-rust-join-line)))

(use-package flycheck-rust
  :straight t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :straight t
  :commands (racer-find-definition racer-describe)
  :hook (rust-mode . racer-mode)
  :config
  (progn
    (add-hook 'racer-mode-hook #'eldoc-mode)

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
  :after rust-mode
  :config
  ;; Enable backtraces in Cargo processes started by Emacs.
  (setenv "RUST_BACKTRACE" "1"))

(use-package rust-faces
  :after rust-mode)

;; Rust backtraces sometimes contain absolute paths from travis builds. Rewrite
;; these to paths relative to the rustup sources directory.

(use-package compile
  :preface
  (progn
    (defun config-rust--rewrite-compilation-buffer (&optional buf &rest _)
      (with-current-buffer (or buf (current-buffer))
        (save-excursion
          (goto-char (or compilation-filter-start (point-min)))
          (let ((inhibit-read-only t)
                (bad-path "/Users/travis/build/rust-lang/rust/"))
            (while (search-forward-regexp (rx-to-string `(or "" ,bad-path)) nil t)
              (replace-match "" t t)))))))
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'compilation-filter-hook #'config-rust--rewrite-compilation-buffer)
    (add-to-list 'compilation-finish-functions #'config-rust--rewrite-compilation-buffer)))

(provide 'config-rust)

;;; config-rust.el ends here
