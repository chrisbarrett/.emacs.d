;;; config-eyebrowse.el --- Configuration for eyebrowse.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package eyebrowse
  :straight t
  :commands (eyebrowse-mode)
  :functions (eyebrowse--delete-window-config
              eyebrowse-switch-to-window-config-0
              eyebrowse-rename-window-config)
  :init
  (defvar eyebrowse-keymap-prefix (kbd "<f10>"))
  :config
  (progn
    (add-to-list 'window-persistent-parameters '(window-side . writable))
    (add-to-list 'window-persistent-parameters '(window-slot . writable))
    (setq eyebrowse-mode-line-style nil)
    (add-hook 'eyebrowse-post-window-switch-hook #'force-mode-line-update)
    (eyebrowse-mode +1)
    ;; Activating eyebrowse dumps you in slot 2 for some reason. :/
    (eyebrowse--delete-window-config 2)
    (eyebrowse--delete-window-config 1)
    (eyebrowse-switch-to-window-config-0)
    (eyebrowse-rename-window-config 0 "default")))

(use-package cb-window-layouts-hydra
  :commands (cb-window-layouts/body)
  :init
  (progn
    (spacemacs-keys-set-leader-keys "v" #'cb-window-layouts/body)
    (with-eval-after-load 'which-key
      (with-no-warnings
        (push `((nil . ,(rx bos "cb-window-layouts/body")) . (nil . "layouts"))
              which-key-replacement-alist)))))

(use-package cb-projectile-eyebrowse
  :commands (cb-projectile-eyebrowse-switch-to-project)
  :init
  (progn
    (with-eval-after-load 'which-key
      (with-no-warnings
        (push `((nil . ,(rx bos "cb-projectile-eyebrowse-switch-to-project")) . (nil . "switch-project"))
              which-key-replacement-alist)))

    (spacemacs-keys-set-leader-keys "pp" #'cb-projectile-eyebrowse-switch-to-project)))

(provide 'config-eyebrowse)

;;; config-eyebrowse.el ends here
