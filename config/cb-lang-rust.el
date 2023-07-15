;;; cb-lang-rust.el --- Rust configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)

(use-package rust-mode :ensure t
  :general
  (:keymaps 'rust-mode-map
   "C-c C-m" 'rust-toggle-mutability)
  :custom
  (rust-format-on-save t))

(mode-leader-set-key :keymaps 'rust-mode-map
  "," '(rustic-popup :wk "rustic...")
  "r" '(rust-run :wk "run")
  "t" '(rust-test :wk "test")
  "c" '(rust-check :wk "typecheck")
  "b" '(rust-compile :wk "build")
  "l" '(rust-run-clippy :wk "clippy"))

(use-package toml-ts-mode :mode ("/Cargo\\.lock\\'")
  :init
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode)))

(use-package rustic :ensure t
  :init
  (use-package inheritenv :demand t :ensure t)
  (use-package evil
    :autoload (evil-set-initial-state)
    :config
    (evil-set-initial-state 'rustic-popup-mode 'emacs))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t))

(use-package rustic-babel :demand t :after ob
  :preface
  ;; Add a `:profile' header arg for specifying the build profile.
  (define-advice rustic-make-process (:filter-args (args) apply-profile)
    (setf (plist-get args :command)
          (append
           (plist-get args :command)
           (when-let* ((profile (alist-get :profile rustic-babel-params)))
             (list (format "--%s" profile)))))
    args))



(defun cb-rust-toggle-pub ()
  "Toggle the `pub' access modifier for the decl at point."
  (interactive)
  (save-match-data
    (save-excursion
      (back-to-indentation)
      (unless (save-excursion
                (looking-at rust-top-item-beg-re))
        (beginning-of-defun)
        (back-to-indentation))
      (cond ((looking-at (rx "pub" symbol-end (* space)))
             (delete-region (match-beginning 0) (match-end 0))
             (message "internal"))
            ((save-excursion
               (back-to-indentation)
               (looking-at rust-top-item-beg-re))
             (goto-char (match-beginning 0))
             (insert "pub")
             (just-one-space)
             (message "public"))
            (t
             (user-error "Cannot toggle pub modifier here"))))))

(general-def :keymaps '(rust-mode-map rustic-mode-map)
  "C-c C-p" 'cb-rust-toggle-pub)

(provide 'cb-lang-rust)

;;; cb-lang-rust.el ends here
