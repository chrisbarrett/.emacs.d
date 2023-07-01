;;; cb-dired.el --- Dired configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (dired-clean-confirm-killing-deleted-buffers nil)
  :general
  (:states 'normal :keymaps 'dired-mode-map
           "M-w" #'dired-copy-filename-as-kill))

(mode-leader-set-key :keymaps 'dired-mode-map
  "," '(dirvish-dispatch :wk "menu")
  "?" '(dired-hide-details-mode :wk "toggle details")
  "." '(dired-omit-mode :wk "toggle hidden")
  "e" '(wdired-change-to-wdired-mode :wk "wdired")
  "s" '(dired-sort-toggle-or-edit :wk "toggle sort")

  "f" 'dired
  "F" '(dired-other-window :wk "dired (other window)")

  "m" '(nil :wk "mark")
  "m a" '(dired-mark-unmarked-files :wk "unmarked")
  "m c" '(dired-change-marks :wk "change")
  "m r" '(dired-mark-files-regexp :wk "by regexp")
  "m l" '(dired-mark-symlinks :wk "symlinks")
  "m d" '(dired-mark-directories :wk "directories")
  "U" '(dired-unmark-all-marks :wk "unmark all")

  "!" '(dired-do-shell-command :wk "shell command...")

  "d" '(nil :wk "execute (marked)")
  "d c" '(dired-do-copy :wk "copy")
  "d D" '(dired-do-delete :wk "delete")
  "d h" '(dired-do-hardlink :wk "hardlink")
  "d s" '(dired-do-relsymlink :wk "symlink (relative)")
  "d S" '(dired-do-symlink :wk "symlink (absolute)")
  "d /" '(dired-do-search :wk "search"))



;;; Dirvish

;; Overhauls dired to provide much better UX

(use-package dirvish
  :hook (after-init . dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(vscode-icon collapse file-size vc-state git-msg ))
  (dirvish-vscode-icon-size 16)
  (dirvish-use-mode-line nil)
  (dirvish-use-header-line t)
  (dirvish-header-line-format '(:left (path) :right (index)))
  (dirvish-use-mode-line nil)
  (dirvish-path-separators '(" ~" " /" " ▸ "))
  :general
  (:keymaps 'dired-mode-map :states 'normal
            [remap quit-window] #'dirvish-quit
            "TAB" #'dirvish-layout-toggle)

  :init
  (use-package vscode-icon
    :defines (vscode-icon-file-alist)
    :config
    (push '("jpg" . "image") vscode-icon-file-alist)))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode)
  (dirvish-directory-view-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package wdired
  :general
  (:states 'normal
           :keymaps 'wdired-mode-map "^" #'evil-first-non-blank
           :keymaps 'dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))

(use-package dired-x
  :demand t
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :general
  (:states 'normal :keymaps 'dired-mode-map "h" #'dired-omit-mode)
  :custom
  (dired-omit-files (rx bol (or "." "__pycache__")))
  (dired-omit-verbose nil)
  (dired-clean-up-buffers-too t))

(provide 'cb-dired)

;;; cb-dired.el ends here
