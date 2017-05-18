;;; cb-magit.el --- Configuration for magit.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'evil-transient-state)

(autoload 'evil-define-key "evil-core")

(use-package magit
  :defer t
  :commands (magit-status magit-blame magit-branch-and-checkout)
  :functions (magit-display-buffer-fullframe-status-v1)
  :preface
  (evil-transient-state-define git-blame
    :title "Git Blame Transient State"
    :doc "
Press [_b_] again to blame further in the history, [_q_] to go up or quit."
    :on-enter (unless (bound-and-true-p magit-blame-mode)
                (call-interactively 'magit-blame))
    :foreign-keys run
    :bindings
    ("b" magit-blame)
    ("q" nil :exit (progn (when (bound-and-true-p magit-blame-mode)
                            (magit-blame-quit))
                          (not (bound-and-true-p magit-blame-mode)))))
  :init
  (spacemacs-keys-set-leader-keys
    "gs" #'magit-status
    "gb" #'git-blame-transient-state/body)
  :config
  (progn
    (evil-define-key 'normal magit-refs-mode-map (kbd ".") #'magit-branch-and-checkout)
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)))

(use-package magithub
  :after magit
  :config
  (progn
    (magithub-feature-autoinject t)
    (setq magithub-api-timeout 5)))

(use-package git-commit-jira-prefix
  :after git-commit
  :commands git-commit-jira-prefix-init
  :config (git-commit-jira-prefix-init))

(use-package evil-magit
  :defer t
  :after magit)

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
