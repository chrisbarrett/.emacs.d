;;; cb-magit.el --- Configuration for magit.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(require 'spacemacs-keys)

(use-package magit
  :defer t
  :commands (magit-status)
  :functions (magit-display-buffer-fullframe-status-v1)
  :init
  (spacemacs-keys-set-leader-keys
    "gs" #'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package git-commit-jira-prefix
  :after git-commit
  :commands git-commit-jira-prefix-init
  :config (git-commit-jira-prefix-init))

(use-package evil-magit
  :defer t
  :after magit)

(use-package magit-browse-repo
  :after magit
  :bind (:map magit-status-mode-map ("&" . magit-browse-repo)))

(use-package git-auto-commit-mode
  :diminish git-auto-commit-mode
  :init
  (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t)))

(provide 'cb-magit)

;;; cb-magit.el ends here
