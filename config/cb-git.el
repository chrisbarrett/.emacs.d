;;; cb-git.el --- Git & VC  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)

(use-package magit
  :general
  (:keymaps 'transient-base-map
            "<escape>" #'transient-quit-one)
  (:states 'normal :keymaps 'magit-refs-mode-map
           "." #'magit-branch-and-checkout)
  (:states '(motion normalm) :keymaps '(magit-section-mode-map magit-status-mode-map)
           [remap evil-next-line] 'next-line
           [remap evil-previous-line] 'previous-line
           [remap evil-next-visual-line] 'next-line
           [remap evil-previous-visual-line] 'previous-line)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-log-section-commit-count 0)
  ;; TODO: Define magit repository dirs
  ;; (magit-repository-directories paths-project-directories)

  ;; Reveal the entire org buffer when blaming or visiting from a diff

  :preface
  (defun cb-git-reveal-org-buffer ()
    (when (derived-mode-p 'org-mode)
      (org-reveal t)))
  :config
  (add-hook 'magit-diff-visit-file-hook 'cb-git-reveal-org-buffer)
  (add-hook 'magit-blame-mode-hook #'cb-git-reveal-org-buffer))

(use-package magit-todos
  :after magit
  :demand t
  :commands (magit-todos-mode)
  :custom
  (magit-todos-ignored-keywords '("NOTE" "DONE" "KLUDGE" "HACK"))
  :config
  (magit-todos-mode +1))

(use-package vc-git
  :custom
  (vc-git-program (executable-find "git")))

(use-package forge
  :after magit
  :demand t
  :custom
  (forge-add-default-bindings nil)
  :config
  (remove-hook 'magit-status-sections-hook 'forge-insert-issues)
  (add-hook 'magit-status-sections-hook 'forge-insert-requested-reviews 90)
  (add-hook 'magit-status-sections-hook 'forge-insert-assigned-issues 90))

(use-package git-auto-commit-mode
  :delight " auto-commit"
  :custom
  (gac-debounce-interval 10)
  (gac-silent-message-p t)
  (gac-automatically-push-p t)
  (gac-automatically-add-new-files-p t)
  :config
  (put 'gac-automatically-push-p 'safe-local-variable 'booleanp))

(use-package vc-annotate
  :general
  (:states 'normal :keymaps 'vc-annotate-mode-map
           "<return>" 'vc-annotate-find-revision-at-line
           "<tab>" 'vc-annotate-goto-line
           "n" 'vc-annotate-next-revision
           "f" 'vc-annotate-next-revision
           "l" 'vc-annotate-show-log-revision-at-line
           "p" 'vc-annotate-prev-revision
           "b" 'vc-annotate-prev-revision
           "d" 'vc-annotate-show-diff-revision-at-line
           "D" 'vc-annotate-show-changeset-diff-revision-at-line
           "." 'vc-annotate-working-revision))

(use-package git-commit-mode
  :preface
  (defun cb-configure-git-commit-mode ()
    (setq-local fill-column 72))
  :init
  (add-hook 'git-commit-mode-hook 'cb-configure-git-commit-mode))

(use-package browse-at-remote
  :general
  ("C-x v o" 'browse-at-remote
   "C-x v y" 'browse-at-remote-kill)
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package git-gutter
  :hook
  (text-mode . git-gutter-mode)
  (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :after git-gutter
  :demand t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(define-derived-mode gitignore-mode conf-unix-mode "Gitignore")
(add-to-list 'auto-mode-alist '("/\\.gitignore\\'" . gitignore-mode))

(define-derived-mode gitmodules-mode conf-unix-mode "Gitmodules")
(add-to-list 'auto-mode-alist '("/\\.gitmodules\\'" . gitmodules-mode))

(provide 'cb-git)

;;; cb-git.el ends here
