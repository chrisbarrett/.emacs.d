;;; config-helm.el --- Configuration for helm.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

;; `helm' is a completion and narrowing framework. I mainly want it for helm-mini.
(use-package helm
  :straight t
  :commands (helm-mini))

;; `helm-posframe' shows helm buffers in a child frame.
(use-package helm-posframe
  :straight t
  :after helm
  ;; KLUDGE: helm posframe seems to close the main frame on linux :/
  :when (equal system-type 'darwin)
  :config
  (progn
    (setq helm-posframe-width 130)
    (setq helm-posframe-height 20)
    (setq helm-posframe-poshandler #'posframe-poshandler-frame-center)
    (helm-posframe-enable)))

;; `helm-org-rifle' provides a really good search interface for org-mode.
(use-package helm-org-rifle
  :straight t
  :commands (helm-org-rifle helm-org-rifle-org-directory)
  :config
  ;; KLUDGE: Ensure the posframe is removed when we exit.
  (add-hook 'helm-org-rifle-after-command-hook 'posframe-hide-all))

;; `helm-projectile' adds a helm buffer for project files and buffers.
(use-package helm-projectile
  :straight t
  :commands (helm-projectile))

(provide 'config-helm)

;;; config-helm.el ends here
