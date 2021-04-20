;;; config-git.el --- Configuration for git.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)



;; Magit provides an excellent suite of interactive commands for working with
;; git.

(use-package magit
  :commands (magit-status magit-blame magit-branch-and-checkout)
  :general (:keymaps 'transient-base-map "<escape>" #'transient-quit-one
            :states 'normal :keymaps 'magit-refs-mode-map "." #'magit-branch-and-checkout)
  :functions (magit-display-buffer-same-window-except-diff-v1)
  :preface
  (progn
    (autoload 'magit-diff-dwim "magit-diff")
    (autoload 'magit-file-relative-name "magit-git")
    (autoload 'magit-find-file "magit-files")
    (autoload 'magit-find-file-other-window "magit-files")
    (autoload 'magit-popup-import-file-args "magit-popup")

    (defun config-git--reveal-org-buffer ()
      (when (derived-mode-p 'org-mode)
        (message "yo")
        (org-reveal t)))

    (defun config-git-find-file (&optional arg)
      (interactive "P")
      (if arg
          (call-interactively #'magit-find-file)
        (call-interactively #'magit-find-file-other-window)))

    (defun config-git--fix-magit-status-header-line ()
      (let ((cookie (face-remap-add-relative 'header-line 'magit-header-line)))
        (face-remap-remove-relative cookie)))

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
    (add-hook 'magit-status-mode-hook #'config-git--fix-magit-status-header-line)
    (add-hook 'magit-blame-mode-hook #'config-git--reveal-org-buffer)
    (setq magit-repository-directories (--map (cons it 1) paths-project-directories))
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-log-section-commit-count 0)))

;; `forge' teaches magit how to display and work with pull requests and issues.

(use-package forge
  :after magit
  :config
  (progn
    (remove-hook 'magit-status-sections-hook 'forge-insert-issues)
    (add-hook 'magit-status-sections-hook 'forge-insert-requested-reviews 90)
    (add-hook 'magit-status-sections-hook 'forge-insert-assigned-issues 90)))

;; This package automatically prepends ticket numbers to commit messages if the
;; current git branch looks like it relates to a ticket.

(use-package git-commit-ticket-prefix
  :hook (git-commit-setup . git-commit-ticket-prefix-insert))

;; evil-magit reconfigures magit keybindings to better support evil.

(use-package evil-magit
  :after (:and magit evil-common)
  :config (evil-magit-init))

;; git-auto-commit-mode provides a mode that automatically commits changes after
;; saving a buffer.

(use-package git-auto-commit-mode
  :commands (git-auto-commit-mode)
  :hook (pass-mode . git-auto-commit-mode)
  :custom ((gac-debounce-interval 10)
           (gac-automatically-push-p t)
           (gac-automatically-add-new-files-p t)))

;; git-commit-mode is used when editing git commit messages.

(use-package git-commit-mode
  :defer t
  :preface
  (defun config-git--configure-git-commit-mode ()
    (setq-local fill-column 72))
  :init
  (add-hook 'git-commit-mode-hook #'config-git--configure-git-commit-mode))

;; Git Time Machine lets you interactively step forward and backwards through a
;; buffers git versions.

(use-package git-timemachine
  :commands (git-timemachine))

;; git-gutter shows git hunk status in buffers.

(use-package git-gutter
  :disabled t
  :hook ((markdown-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode)
         (conf-mode . git-gutter-mode))
  :init
  :custom ((git-gutter:disabled-modes '(org-mode asm-mode image-mode))
           (git-gutter:update-interval 1)
           (git-gutter:window-width 2)
           (git-gutter:ask-p nil)))

;; git-gutter-fringe moves the git-gutter into the fringe, so it doesn't
;; interact with buffer text.

(use-package git-gutter-fringe
  :disabled t
  :after git-gutter
  :init
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  :config
  (progn
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)))

;; Magit-gpg is a hack to show GPG signing status in magit's commit info.

(use-package magit-gpg
  :after magit
  :preface
  (progn
    (autoload 'magit-add-section-hook "magit")
    (autoload 'magit-insert-revision-headers "magit")
    (autoload 'magit-gpg-insert-revision-gpg "magit-gpg"))
  :config
  (magit-add-section-hook 'magit-revision-sections-hook
                          #'magit-gpg-insert-revision-gpg
                          #'magit-insert-revision-headers
                          t))

;; pretty-magit adds pretty fontification to magit buffers.

(use-package pretty-magit
  :after magit
  :preface
  (autoload 'pretty-magit-add-leader "pretty-magit")
  :config
  (progn
    (defconst config-git-jira-projects '("CAPPS"))

    (pretty-magit-add-leader
     (rx-to-string `(group (+ space) (or ,@config-git-jira-projects) "-" (+ digit) symbol-end))
     nil '(face magit-hash)
     '(magit-log-mode))))

;; VC annotate happens to be a nice way to view file changes.

(use-package vc-annotate
  :commands (vc-annotate)
  :general
  (:states 'normal :keymaps 'vc-annotate-mode-map
   "n" 'vc-annotate-next-revision
   "f" 'vc-annotate-next-revision
   "p" 'vc-annotate-prev-revision
   "b" 'vc-annotate-prev-revision
   "." 'vc-annotate-working-revision))

;; `browse-at-remote' provides commands for opening the current buffer in the
;; source repo.

(use-package browse-at-remote
  :commands (browse-at-remote))

(provide 'config-git)

;;; config-git.el ends here
