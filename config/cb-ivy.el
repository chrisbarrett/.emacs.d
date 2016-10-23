;;; cb-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(require 'spacemacs-keys)

(use-package ivy
  :leader-bind
  (("r" . ivy-resume)
   ("b b" . ivy-switch-buffer))

  :preface
  (progn
    (autoload 'ivy-mode "ivy")
    (use-package cb-ivy-continue-with-input :commands (cb-ivy-continue-with-input)))

  :init
  (progn
    (bind-key "C-c C-r" #'ivy-resume)
    (bind-key "C-x b" #'ivy-switch-buffer))

  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (define-key ivy-minibuffer-map (kbd "ESC") #'keyboard-escape-quit)
    (define-key ivy-minibuffer-map (kbd "S-<return>") #'cb-ivy-continue-with-input)
    (ivy-mode))

  :defines (ivy-use-virtual-buffers ivy-count-format))


(use-package counsel
  :preface
  (progn
    (autoload 'ivy-immediate-done "ivy")
    (autoload 'counsel-mode "counsel"))

  :leader-bind
  (("SPC" . counsel-M-x)
   ("f f" . counsel-find-file)
   ("h d f" . counsel-describe-function)
   ("h d v" . counsel-describe-variable))

  :init
  (progn
    (bind-key "M-x" #'counsel-M-x)
    (bind-key "C-x C-f" #'counsel-find-file)
    (bind-key "C-h v" #'counsel-describe-variable)
    (bind-key "C-h f" #'counsel-describe-function))

  :config
  (progn
    (define-key counsel-find-file-map (kbd "C-M-j") #'ivy-immediate-done)
    (counsel-mode +1))

  :functions (counsel-expression-history))

(provide 'cb-ivy)

;;; cb-ivy.el ends here
