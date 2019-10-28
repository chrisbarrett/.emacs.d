;;; config-workspaces.el --- Configuration for workspace functionality.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package eyebrowse
  :straight t
  :hook (after-init . eyebrowse-mode)
  :preface
  (progn
    (defun config-workspaces-create-window-config (name)
      (interactive (list (let ((current-tags (--map (nth 2 it) (eyebrowse--get 'window-configs)))
                               input)
                           (while (progn
                                    (setq input (read-string "New workspace name: " (ignore-errors (projectile-project-name))))
                                    (cond ((string-blank-p input)
                                           (message "Input must be non-empty")
                                           (sit-for 1)
                                           t)
                                          ((-contains-p current-tags input)
                                           (message "Name must be unique")
                                           (sit-for 1)
                                           t))))
                           input)))
      (eyebrowse-create-window-config)
      (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) name))

    (defun config-workspaces-close-window-config ()
      (interactive)
      (if (y-or-n-p (format "Close window config %s (%s)? "
                            (eyebrowse--get 'current-slot)
                            (nth 2 (assoc (eyebrowse--get 'current-slot)
                                          (eyebrowse--get 'window-configs)))))
          (call-interactively #'eyebrowse-close-window-config)
        (user-error "Aborted"))))
  :general
  (:states '(normal motion)
   "g ," #'eyebrowse-prev-window-config
   "g ." #'eyebrowse-next-window-config
   "g SPC" #'eyebrowse-switch-to-window-config
   "g TAB" #'eyebrowse-last-window-config
   "g c" #'config-workspaces-create-window-config
   "g x" #'config-workspaces-close-window-config)
  :custom
  ((eyebrowse-mode-line-separator " "))
  :config
  (with-eval-after-load 'evil
    (eyebrowse-setup-evil-keys)))

(provide 'config-workspaces)

;;; config-workspaces.el ends here
