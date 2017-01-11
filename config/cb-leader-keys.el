;;; cb-leader-keys.el --- Grab-bag for configuring general prefixed keys.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)

(autoload 'evil-window-rotate-downwards "evil-commands")
(autoload 'cb/alternate-buffer "cb-alternate-buffer")
(autoload 'cb/copy-buffer-path "cb-copy-buffer-path")
(autoload 'cb/rename-file-and-buffer "cb-rename-file-and-buffer")
(autoload 'cb/sudo-edit "cb-sudo-edit")
(autoload 'cb/toggle-window-split "cb-toggle-window-split")
(autoload 'cb-goto-init-file "cb-goto")
(autoload 'cb-goto-messages "cb-goto")
(autoload 'cb-goto-personal-config "cb-goto")

(use-package cb-delete-current-buffer-and-file
  :commands (cb/delete-current-buffer-and-file)
  :preface
  (progn
    (autoload 'projectile-invalidate-cache "projectile")
    (autoload 'projectile-project-p "projectile")

    (defun cb-leader-keys--invalidate-cache (_path)
      (when (and (featurep 'projectile) (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache))))

  :config
  (add-hook 'cb-delete-current-buffer-and-file-functions #'cb-leader-keys--invalidate-cache))

(use-package spacemacs-keys
  :preface
  (progn
    (autoload 'evil-window-next "evil-commands")
    (autoload 'evil-window-split "evil-commands")
    (autoload 'evil-window-vsplit "evil-commands")

    (defun cb-leader-keys/reload-file ()
      "Revisit the current file."
      (interactive)
      (when-let (path (buffer-file-name))
        (find-alternate-file path))))

  :config
  (progn
    (define-key universal-argument-map (kbd (concat "SPC u")) #'universal-argument-more)

    (spacemacs-keys-set-leader-keys
      "u"   #'universal-argument
      "SPC" #'execute-extended-command
      "TAB" #'cb/alternate-buffer
      "|"   #'cb/toggle-window-split

      "!"   #'shell-command

      "b d" #'kill-this-buffer
      "b b" #'bury-buffer
      "b v" #'cb-leader-keys/reload-file

      "C" #'compile

      "c r" #'comment-or-uncomment-region

      "f D" #'cb/delete-current-buffer-and-file
      "f F" #'find-file-other-window
      "f R" #'cb/rename-file-and-buffer
      "f e" #'cb/sudo-edit
      "f f" #'find-file
      "f s" #'save-buffer
      "f v" #'cb-leader-keys/reload-file
      "f y" #'cb/copy-buffer-path

      "g i" #'cb-goto-init-file
      "g m" #'cb-goto-messages
      "g p" #'cb-goto-personal-config

      "h d c" #'describe-face
      "h d k" #'describe-key
      "h d m" #'describe-mode
      "h f c" #'find-face-definition
      "h f f" #'find-function
      "h f l" #'find-library
      "h f v" #'find-variable
      "h i"   #'info

      "k b" #'kill-this-buffer
      "k w" #'delete-window

      "q" #'delete-window

      "w =" #'balance-windows
      "w w" #'evil-window-next
      "w o" #'delete-other-windows
      "w q" #'delete-window
      "w r" #'evil-window-rotate-downwards
      "w -" #'evil-window-split
      "w /" #'evil-window-vsplit)))

(use-package cb-scale-font-transient-state
  :commands (cb-scale-font-transient-state/body)
  :init
  (spacemacs-keys-set-leader-keys
    "zx" #'cb-scale-font-transient-state/body))

(use-package cb-buffer-transient-state
  :commands (cb-buffer-transient-state/body
             cb-buffer-transient-state/next-buffer
             cb-buffer-transient-state/previous-buffer)
  :init
  (spacemacs-keys-set-leader-keys
    "bn" #'cb-buffer-transient-state/next-buffer
    "bN" #'cb-buffer-transient-state/previous-buffer
    "bp" #'cb-buffer-transient-state/previous-buffer))


(provide 'cb-leader-keys)

;;; cb-leader-keys.el ends here
