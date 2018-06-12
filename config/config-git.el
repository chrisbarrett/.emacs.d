;;; config-git.el --- Configuration for git.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'config-hydras)
(require 'paths)



;; Magit provides an excellent suite of interactive commands for working with
;; git.

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
          (user-error "Buffer isn't visiting a file"))))))
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

;; Magithub adds some basic GitHub features to magit.

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

;; This package automatically prepends JIRA ticket numbers to commit messages if
;; the current git branch looks like it relates to a JIRA ticket.

(use-package git-commit-jira-prefix
  :hook (git-commit-setup . git-commit-jira-prefix-insert))

;; evil-magit reconfigures magit keybindings to better support evil.

(use-package evil-magit
  :straight t
  :after (:and magit evil-common)
  :config (evil-magit-init))

(use-package deferred :straight t)

;; git-auto-commit-mode provides a mode that automatically commits changes after
;; saving a buffer.

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

;; Git Time Machine lets you interactively step forward and backwards through a
;; buffers git versions.

(use-package git-timemachine
  :straight (:host gitlab :repo "pidu/git-timemachine")
  :defer t)

;; Diff-hl highlights changed file hunks in the fringe of the window, and
;; provides commands for working with hunks.

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

;; Magit-gpg is a hack to show GPG signing status in magit's commit info.

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
