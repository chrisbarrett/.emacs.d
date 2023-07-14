;;; cb-completion.el --- Configuration for completion engines.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-max-saved-items 100)
  (recentf-exclude (list (rx "TAGS" eos)
                         (rx ".DS_Store" eos)
                         (rx "." (or "gz" "zip" "tar" "7z") eos)
                         (rx bos "/sudo:root@")
                         (rx "/.git/")
                         (rx "/" (or "build" "dist" "target") "/")
                         (rx (or "/var/folders/"
                                 "/usr/local/Cellar/"
                                 "/tmp/"
                                 "/nix/store/"))
                         'file-remote-p
                         (regexp-quote no-littering-etc-directory)
                         (regexp-quote no-littering-var-directory))))

(setq completion-ignore-case t)
(setq completion-cycle-threshold 3)

;; Perform both indentation & text completion with TAB.
(setq tab-always-indent 'complete)

;; Emacs by default ignores a ton of stuff, but most of the entries don't matter
;; to me.
(setq completion-ignored-extensions
      '(
        ;; VC
        ".git/"
        ;; OS-generated
        ".DS_Store"
        ".o" "~" ".bin" ".lbin" ".so" ".a"
        ;; Emacs generated
        ".elc" ".eln"
        ;; Nix, direnv, etc
        ".drv"
        ".direnv/"
        ))



;; Increase minibuffer history length.

(setq history-length 1000)

;; Remove any lingering *completions* buffer on minibuffer exit
(defun cb--cleanup-completions-buffer ()
  (when-let* ((buf (get-buffer "*Completions*")))
    (kill-buffer buf)))

(add-hook 'minibuffer-exit-hook #'cb--cleanup-completions-buffer)



(use-package simple

  ;; M-n beyond the end in `completing-read' will use the thing at point
  :preface
  (autoload 'ffap-guesser "ffap")
  (autoload 'thing-at-point-url-at-point "thingatpt")
  (defun cb-minibuffer-default-add-function ()
    (with-selected-window (minibuffer-selected-window)
      (delete-dups
       (delq nil
             (list (thing-at-point 'symbol)
                   (thing-at-point 'list)
                   (ffap-guesser)
                   (thing-at-point-url-at-point))))))
  :custom
  (minibuffer-default-add-function #'cb-minibuffer-default-add-function)

  :custom
  ;; Hide commands irrelevant to current mode from M-x
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package crm
  ;; Show additional information in prompt if completing-read-multiple is used
  :preface
  (define-advice completing-read-multiple (:filter-args (args) show-indicator)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args))))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)

  ;; Enable for eval-expression, etc.
  :preface
  (defun cb-corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  :hook (minibuffer-setup . cb-corfu-enable-in-minibuffer)

  ;; Exiting `corfu' also returns to evil normal state
  :preface
  (defun config-corfu-quit-and-enter-normal-state ()
    (interactive)
    (corfu-quit)
    (when (evil-insert-state-p)
      (evil-normal-state)))
  :general
  (:keymaps 'corfu-map
   [remap corfu-quit] 'config-corfu-quit-and-enter-normal-state))

;; Use a less aggressive completion configuration in =eshell=
(use-package eshell
  :after corfu
  :demand t
  :init
  (defun cb-config-corfu-eshell-setup ()
    (setq-local corfu-auto nil)
    (corfu-mode +1))
  :hook (eshell-mode . cb-config-corfu-eshell-setup))

(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :hook (after-init . vertico-mode)
  :general (:keymaps 'vertico-map
            "C-<return>" 'vertico-exit-input
            "M-<return>" 'minibuffer-force-complete-and-exit)
  :custom
  (vertico-cycle t)
  :preface
  (define-advice vertico--exhibit (:around (fn &rest args) ignore-errors)
    (ignore-errors
      (apply fn args))))

(use-package vertico-directory
  :after vertico
  :demand t
  :general (:keymaps 'vertico-map
            "RET" 'vertico-directory-enter
            "DEL" 'vertico-directory-delete-char
            "C-h" 'vertico-directory-delete-word)
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :general (:keymaps 'corfu-map
            "M-n" 'corfu-popupinfo-scroll-up
            "M-p" 'corfu-popupinfo-scroll-down
            "<f1>" 'corfu-popupinfo-toggle))

(use-package kind-icon
  :after corfu
  :demand t
  :autoload (kind-icon-reset-cache)
  :custom
  (kind-icon-use-icons nil)
  (kind-icon-default-face 'corfu-default)
  (corfu-margin-formatters '(kind-icon-margin-formatter))
  :config
  (add-hook 'after-load-theme-functions (lambda (_)
                                          (kind-icon-reset-cache))))

(use-package cape
  :general ("M-/" 'completion-at-point)
  :demand t
  :preface
  (defun cb-cape-default-setup ()
    (add-to-list 'completion-at-point-functions 'cape-file)
    (add-to-list 'completion-at-point-functions 'cape-dabbrev)
    (add-to-list 'completion-at-point-functions 'cape-tex)
    (add-to-list 'completion-at-point-functions 'cape-rfc1345)
    (add-to-list 'completion-at-point-functions 'cape-keyword))

  (defun cb-cape-lisp-setup ()
    (make-local-variable 'completion-at-point-functions)
    (cb-cape-default-setup)
    (add-to-list 'completion-at-point-functions 'cape-symbol))

  (defun cb-cape-text-mode-setup ()
    (make-local-variable 'completion-at-point-functions)
    (cb-cape-default-setup)
    (add-to-list 'completion-at-point-functions 'cape-ispell))

  :hook
  (emacs-lisp-mode . cb-cape-lisp-setup)
  (lisp-data-mode . cb-cape-lisp-setup)
  (ielm-mode . cb-cape-lisp-setup)
  (text-mode .  cb-cape-text-mode-setup)

  :init
  (cb-cape-default-setup))

(use-package historian
  :hook (after-init . historian-mode)
  :preface
  (define-advice historian-save (:around (f &rest args) force-text-encoding)
    "Fix text encoding issues."
    (let ((coding-system-for-write 'binary))
      (apply f args))))

(provide 'cb-completion)

;;; cb-completion.el ends here
