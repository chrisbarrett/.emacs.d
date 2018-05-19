;;; config-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'noflet)
(require 'paths)
(require 'subr-x)

(use-package ivy
  :straight t
  :commands (ivy-occur ivy-help)
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)

   :map ivy-occur-mode-map
   ("C-x C-w" . ivy-wgrep-change-to-wgrep-mode)

   :map ivy-minibuffer-map
   ("C-z" . ivy-dispatching-done)
   ("C-l" . ivy-partial-or-done)
   ("C-<return>" . ivy-immediate-done)

   ;; Browse read-expression histroy with ivy
   :map read-expression-map
   ("C-r" . counsel-expression-history))

  :preface
  (progn
    ;; KLUDGE: Declare dynamic var.
    (defvar org-startup-folded)

    (defun config-ivy-help ()
      (interactive)
      (let ((org-startup-folded 'nofold))
        (ivy-help)
        (pop-to-buffer (get-buffer "*Ivy Help*"))))

    (defun config-ivy-with-empty-ivy-extra-directories (f &rest args)
      (let ((ivy-extra-directories nil))
        (apply f args)))

    ;; Define a command for entering wgrep straight from ivy results.

    (defun ivy-occur-then-wgrep ()
      "Shortcut for calling `ivy-occur' then activating wgrep."
      (interactive)
      (noflet
        ;; HACK: Run the original exit callback, then assume the occur buffer is
        ;; being displayed and change to wgrep.
        ((ivy-exit-with-action
          (action)
          (funcall this-fn (lambda (&rest args)
                             (apply action args)
                             (ivy-wgrep-change-to-wgrep-mode)))))
        (ivy-occur))))

  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
    (setq ivy-magic-slash-non-match-action nil)

    ;; Do not show extra directories when finding files.
    (setq ivy-extra-directories '("."))
    (advice-add #'counsel-find-file :around #'config-ivy-with-empty-ivy-extra-directories)

    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "<f1>") #'config-ivy-help)
    (define-key ivy-minibuffer-map (kbd "C-c C-e") #'ivy-occur-then-wgrep)

    ;; Increase the maximum number of candidates that will be sorted
    ;; using `flx'. The default is 200, which means `flx' is almost
    ;; never used. Setting it too high (e.g. 10000) causes lag. This
    ;; seems to be a good compromise (for example, @PythonNut uses it,
    ;; see [1]).
    ;;
    ;; [1]: https://github.com/PythonNut/emacs-config/blob/c8bff5cce293006ec5cdc39a86982431a758a9a0/modules/config-ivy.el#L68
    (setq ivy-flx-limit 2000)

    (ivy-mode)))

(use-package flx
  :straight t
  :after ivy)

(use-package ivy-hydra
  :after ivy
  :straight t)

(use-package swiper
  :straight t
  :bind (:map evil-normal-state-map ("/" . swiper)))

(use-package counsel
  :straight t
  :demand t

  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)

         :map counsel-find-file-map
         ("C-M-j" . ivy-immediate-done)

         :map
         counsel-find-file-map
         ("C-h" . counsel-up-directory))

  :preface
  (progn
    (autoload 'ivy-immediate-done "ivy")
    (defun config-ivy--populate-with-symbol-at-point (f &rest args)
      (if-let ((sym (symbol-at-point)))
          (apply f (symbol-name sym) (cdr args))
        (apply f args))))

  :config
  (progn
    (setq counsel-yank-pop-separator
          (propertize "\n-------------------------------------------------\n"
                      'face '(:foreground "gray50")))

    ;; Prefill counsel-ag with the symbol at point.
    (advice-add 'counsel-rg :around #'config-ivy--populate-with-symbol-at-point)
    (advice-add 'counsel-ag :around #'config-ivy--populate-with-symbol-at-point)

    (counsel-mode +1)))

;; Remembers your choices in completion menus.
(use-package historian
  :straight t
  :demand t
  :config
  (historian-mode +1))

;; Uses Historian to sort Ivy candidates by frecency+flx.
(use-package ivy-historian
  :straight t
  :after ivy
  :config
  (progn
    ;; Tweak historian weighting settings. These values are chosen
    ;; subjectively to produce good results.
    (setq ivy-historian-freq-boost-factor 2000)
    (setq ivy-historian-recent-boost 2000)
    (setq ivy-historian-recent-decrement 1000)

    (ivy-historian-mode 1)))

(provide 'config-ivy)

;;; config-ivy.el ends here
