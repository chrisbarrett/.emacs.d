;;; config-dired.el --- Configuration for dired.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)
(require 'paths)

(major-mode-hydra-define dired-mode nil
  ("Toggle"
   (("?" dired-hide-details-mode "file flags")
    ("." dired-omit-mode "hidden files")
    ("e" wdired-change-to-wdired-mode "edit (wdired)")
    ("s" dired-sort-toggle-or-edit "sort by date"))

   "Navigate"
   (("g" dired "change directory")
    ("o" dired-other-window "dired-other-window"))

   "Mark"
   (("ma" dired-mark-unmarked-files "unmarked files")
    ("mc" dired-change-marks "change")
    ("mr" dired-mark-files-regexp "by regex")
    ("ml" dired-mark-symlinks "symlinks")
    ("md" dired-mark-directories "dirs")
    ("U" dired-unmark-all-marks "unmark all"))

   "Mark Actions"
   (("!" dired-do-shell-command "shell command")
    ("dc" dired-do-copy "copy")
    ("dD" dired-do-delete "delete")
    ("dh" dired-do-hardlink "hardlink")
    ("ds" dired-do-relsymlink "symlink (relative)")
    ("dS" dired-do-symlink "symlink (absolute)")
    ("dq" dired-do-search "search inside"))))

(use-package dired
  :commands (dired dired-hide-details-mode)
  :defer t
  :general
  (:states 'normal :keymaps 'dired-mode-map "$" #'end-of-line)
  :preface
  (progn
    ;; Needs to be set before dired+ loads.
    (general-setq dired-omit-files (rx bol (or (+ ".")
                                               (and "__pycache__" eol))))

    (defun config-dired--sort-directories-first (&rest _)
      "Sort dired listings with directories first."
      (save-excursion
        (let (buffer-read-only)
          (forward-line 2) ;; beyond dir. header
          (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
        (set-buffer-modified-p nil))))

  :config
  (progn
    (add-hook 'dired-mode-hook #'hl-line-mode)

    (put 'dired-find-alternate-file 'disabled nil)

    (setq-default dired-listing-switches "-alhv")
    (general-setq dired-dwim-target t)
    (general-setq dired-hide-details-hide-symlink-targets nil)
    (advice-add 'dired-readin :after #'config-dired--sort-directories-first)

    ;; Instantly revert Dired buffers on re-visiting them, with no
    ;; message.
    (general-setq dired-auto-revert-buffer t)))

(use-package dired+
  :defer t
  :hook (dired-mode . dired-hide-details-mode)
  :general
  (:states 'normal :keymaps 'dired-mode-map
   "j" #'diredp-next-line
   "k" #'diredp-previous-line)
  :config
  (setq diredp-wrap-around-flag nil))

(use-package dired-x
  :defer t
  :hook (dired-mode . dired-omit-mode)
  :general
  (:states 'normal :keymaps 'dired-mode-map "h" #'dired-omit-mode)
  :config
  (progn
    (general-setq dired-omit-verbose nil)
    (general-setq dired-clean-up-buffers-too t)))

(use-package wdired
  :defer t
  :preface
  (autoload 'evil-first-non-blank "evil-commands")
  :general
  (:states 'normal
   :keymaps 'wdired-mode-map "^" #'evil-first-non-blank
   :keymaps 'dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))

(provide 'config-dired)

;;; config-dired.el ends here
