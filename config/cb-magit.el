;;; cb-magit.el --- Configuration for magit.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(require 'spacemacs-keys)
(require 'evil-transient-state)

(use-package magit
  :defer t
  :commands (magit-status magit-blame)
  :functions (magit-display-buffer-fullframe-status-v1)
  :init
  (spacemacs-keys-set-leader-keys
    "gs" #'magit-status
    "gb" #'magit-blame)
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
  :commands (git-auto-commit-mode)
  :init
  (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t)))

(use-package git-timemachine
  :defer t
  :commands
  (git-timemachine
   git-timemachine-show-current-revision
   git-timemachine-show-nth-revision
   git-timemachine-show-previous-revision
   git-timemachine-show-next-revision
   git-timemachine-show-previous-revision
   git-timemachine-kill-revision
   git-timemachine-quit)
  :preface
  (evil-transient-state-define time-machine
    :title "Git Timemachine Transient State"
    :doc "
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
        :on-enter (unless (bound-and-true-p git-timemachine-mode)
                      (call-interactively 'git-timemachine))
        :on-exit (when (bound-and-true-p git-timemachine-mode)
                   (git-timemachine-quit))
        :foreign-keys run
        :bindings
        ("c" git-timemachine-show-current-revision)
        ("g" git-timemachine-show-nth-revision)
        ("p" git-timemachine-show-previous-revision)
        ("n" git-timemachine-show-next-revision)
        ("N" git-timemachine-show-previous-revision)
        ("Y" git-timemachine-kill-revision)
        ("q" nil :exit t))
  :init
  (spacemacs-keys-set-leader-keys
    "gt" #'time-machine-transient-state/body))

(provide 'cb-magit)

;;; cb-magit.el ends here
