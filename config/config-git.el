;;; config-git.el --- Configuration for git.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'config-hydras)
(require 'evil-transient-state)
(require 'paths)



(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . conf-unix-mode))

(use-package magit
  :straight t
  :commands (magit-status magit-blame magit-branch-and-checkout)
  :general (:states 'normal :keymaps 'magit-refs-mode-map "." #'magit-branch-and-checkout)
  :functions (magit-display-buffer-fullframe-status-v1)
  :preface
  (progn
    (autoload 'magit-diff-dwim "magit-diff")
    (autoload 'magit-file-relative-name "magit-git")
    (autoload 'magit-find-file "magit-files")
    (autoload 'magit-find-file-other-window "magit-files")
    (autoload 'magit-popup-import-file-args "magit-popup")

    (defun config-git-find-file (&optional arg)
      (interactive "P")
      (if arg
          (call-interactively #'magit-find-file)
        (call-interactively #'magit-find-file-other-window)))

    (defun config-git-diff-buffer-file (&optional arg)
      (interactive "P")
      (let* ((file (magit-file-relative-name))
             (magit-diff-arguments
              (when file
                (magit-popup-import-file-args (default-value 'magit-diff-arguments) (list file)))))
        (cond
         (arg
          (call-interactively #'magit-diff-buffer-file-popup))
         (file
          (call-interactively #'magit-diff))
         (t
          (user-error "Buffer isn't visiting a file")))))

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
                            (not (bound-and-true-p magit-blame-mode))))))
  :config
  (progn
    (config-hydras-insinuate magit-mode-map)
    (setq magit-repository-directories
          '(("~/Documents" . 1)
            ("~/Projects" . 1)
            ("~/Sync" . 1)
            ("~/workspace" . 1)))
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (setq magit-log-section-commit-count 0)))

(use-package magithub
  :straight t
  :after magit
  :defer t
  :init
  ;; HACK: Fix reference to removed function.
  (defalias 's-blank-p #'s-blank?)
  :config
  (progn
    (require 'magithub-completion)
    (magithub-feature-autoinject t)
    (setq magithub-clone-default-directory "~/Projects")))

(use-package git-commit-jira-prefix
  :hook (git-commit-setup . git-commit-jira-prefix-insert))

(use-package evil-magit
  :straight t
  :after (:and magit evil)
  :config
  (evil-magit-init))

(use-package deferred :straight t)

(use-package git-auto-commit-mode
  :straight t
  :commands (git-auto-commit-mode)
  :init
  (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))
  :preface
  (progn
    (defun config-git--maybe-commit-and-push ()
      (let ((file (convert-standard-filename (file-name-nondirectory (buffer-file-name)))))
        (deferred:try
          (deferred:$
            (deferred:process "git" "add" (shell-quote-argument file))
            (deferred:processc it "git" "commit" "-m" (shell-quote-argument (gac--commit-msg (buffer-file-name)))))
          :catch #'ignore
          :finally #'gac-push))))

  :config
  (defalias 'gac-after-save-func #'config-git--maybe-commit-and-push))

(use-package git-timemachine
  :straight t
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
  (evil-transient-state-define git-time-machine
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
    ("q" nil :exit t)))

(use-package diff-hl
  :straight t
  :after magit
  :commands (diff-hl-magit-post-refresh
             global-diff-hl-mode
             diff-hl-next-hunk
             diff-hl-previous-hunk
             diff-hl-revert-hunk
             diff-hl-goto-hunk)
  :preface
  (progn
    (defun config-git--diff-hl-mode-on ()
      (diff-hl-mode -1))

    (defun config-git--diff-hl-mode-off ()
      (diff-hl-mode +1)))
  :config
  (progn
    ;; Diff-hl interferes with iedit. Disable diff-hl temporarily while iedit is
    ;; enabled.
    (add-hook 'iedit-mode-hook #'config-git--diff-hl-mode-on)
    (add-hook 'iedit-mode-end-hook #'config-git--diff-hl-mode-off)

    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (global-diff-hl-mode +1)))

(use-package magit-gpg
  :after magit
  :functions (magit-gpg-insert-revision-gpg)
  :config
  (magit-add-section-hook 'magit-revision-sections-hook
                          #'magit-gpg-insert-revision-gpg
                          #'magit-insert-revision-headers
                          t))

(provide 'config-git)

;;; config-git.el ends here
