;;; cb-leader-keys.el --- Grab-bag for configuring general prefixed keys.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(use-package cb-alternate-buffer
  :commands (cb/alternate-buffer))

(use-package cb-copy-buffer-path
  :commands (cb/copy-buffer-path))

(use-package cb-goto
  :leader-bind
  (("g i" . cb-goto-init-file)
   ("g m" . cb-goto-messages)))

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
  (autoload 'spacemacs-keys-set-leader-keys "spacemacs-keys")

  :config
  (spacemacs-keys-set-leader-keys
    "SPC" #'execute-extended-command
    "TAB" #'cb/alternate-buffer

    "b d" #'kill-this-buffer

    "f f" #'find-file
    "f F" #'find-file-other-window
    "f s" #'save-buffer
    "f D" #'cb/delete-current-buffer-and-file
    "f y" #'cb/copy-buffer-path

    "w =" #'balance-windows
    "w w" #'evil-window-next
    "w o" #'delete-other-windows
    "w -" #'evil-window-split
    "w /" #'evil-window-vsplit))

(provide 'cb-leader-keys)

;;; cb-leader-keys.el ends here
