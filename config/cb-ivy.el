;;; cb-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)

(use-package ivy
  :commands (ivy-dispatching-done
             ivy-help
             ivy-immediate-done
             ivy-mode
             ivy-partial-or-done
             ivy-resume
             ivy-switch-buffer
             ivy-wgrep-change-to-wgrep-mode)

  :preface
  (progn
    (autoload 'cb-ivy-occur-then-wgrep "cb-ivy-occur-then-wgrep")

    ;; KLUDGE: Declare dynamic var.
    (defvar org-startup-folded)

    (defun cb-ivy-help ()
      (interactive)
      (let ((org-startup-folded 'nofold))
        (ivy-help)
        (pop-to-buffer (get-buffer "*Ivy Help*"))))

    (defun cb-ivy-with-empty-ivy-extra-directories (f &rest args)
      (let ((ivy-extra-directories nil))
        (apply f args))))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "r" #'ivy-resume
      "b s" #'ivy-switch-buffer)

    (bind-key "C-c C-r" #'ivy-resume)
    (bind-key "C-x b" #'ivy-switch-buffer))

  :config
  (progn
    (require 'flx)
    (require 'ivy-hydra)

    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
    (setq ivy-magic-slash-non-match-action nil)

    ;; Do not show extra directories when finding files.
    (setq ivy-extra-directories '("."))
    (advice-add #'counsel-find-file :around #'cb-ivy-with-empty-ivy-extra-directories)

    (define-key ivy-minibuffer-map (kbd "<f1>") #'cb-ivy-help)
    (define-key ivy-occur-mode-map (kbd "C-x C-w") #'ivy-wgrep-change-to-wgrep-mode)
    (define-key ivy-minibuffer-map (kbd "C-z") #'ivy-dispatching-done)
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-partial-or-done)
    (define-key ivy-minibuffer-map (kbd "C-c C-e") #'cb-ivy-occur-then-wgrep)
    (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-immediate-done)

    (ivy-mode))

  :defines (ivy-use-virtual-buffers ivy-count-format))

(use-package counsel
  :demand t

  :bind (("C-s" . counsel-grep-or-swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function))

  :commands (counsel-descbinds
             counsel-expression-history
             counsel-imenu
             counsel-recentf
             counsel-yank-pop)
  :preface
  (progn
    (autoload 'ivy-immediate-done "ivy")
    (autoload 'counsel-up-directory "counsel")
    (autoload 'counsel-mode "counsel")

    (defun cb-ivy--populate-with-symbol-at-point (f &rest args)
      (if-let ((sym (symbol-at-point)))
          (apply f (symbol-name sym) (cdr args))
        (apply f args))))

  :init
  (spacemacs-keys-set-leader-keys
    "SPC" #'counsel-M-x
    "?"   #'counsel-descbinds
    "f f" #'counsel-find-file
    "f r" #'counsel-recentf
    "k r" #'counsel-yank-pop
    "i"   #'counsel-imenu
    "h d f" #'counsel-describe-function
    "h d v" #'counsel-describe-variable)

  :config
  (progn
    (define-key counsel-find-file-map (kbd "C-M-j") #'ivy-immediate-done)
    (define-key counsel-find-file-map (kbd "C-h") #'counsel-up-directory)

    (evil-global-set-key 'normal "/" #'counsel-grep-or-swiper)

    (defface cb-counsel-separator
      '((t :foreground "gray50"))
      "Face for "
      :group 'cb-ivy)


    (setq counsel-yank-pop-separator (propertize "\n-------------------------------------------\n"
                                                 'face 'cb-counsel-separator))

    ;; Prefill counsel-ag with the symbol at point.
    (advice-add 'counsel-rg :around #'cb-ivy--populate-with-symbol-at-point)
    (advice-add 'counsel-ag :around #'cb-ivy--populate-with-symbol-at-point)

    (counsel-mode +1)))

(provide 'cb-ivy)

;;; cb-ivy.el ends here
