;;; config-dired.el --- Configuration for dired.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-major-mode-hydra)
(require 'paths)
(require 'spacemacs-keys)

(autoload 'evil-define-key "evil")



(cb-major-mode-hydra-define dired-mode
  "Toggle"
  (("?" dired-hide-details-mode "file flags")
   ("." dired-omit-mode "hidden files")
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
   ("ds" dired-do-search "symlink (relative)")
   ("dS" dired-do-search "symlink (absolute)")
   ("dq" dired-do-search "search inside")))



(use-package dired
  :defer t
  :commands (dired dired-hide-details-mode)
  :bind (:map spacemacs-keys-default-map ("d" . dired))
  :preface
  (progn
    (autoload 'diredp-next-line "dired+")
    (autoload 'diredp-previous-line "dired+")

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
    (setq dired-dwim-target t)
    (setq dired-hide-details-hide-symlink-targets nil)
    (advice-add 'dired-readin :after #'config-dired--sort-directories-first)

    ;; Instantly revert Dired buffers on re-visiting them, with no
    ;; message.
    (setq dired-auto-revert-buffer t)

    (evil-define-key 'normal dired-mode-map (kbd "$") #'end-of-line)
    (evil-define-key 'normal dired-mode-map (kbd "j") #'diredp-next-line)
    (evil-define-key 'normal dired-mode-map (kbd "k") #'diredp-previous-line)))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :init
  (add-hook 'dired-load-hook (lambda () (load "dired-x")))
  :config
  (progn
    (evil-define-key 'normal dired-mode-map (kbd "h") #'dired-omit-mode)
    (setq dired-omit-verbose nil)
    (setq dired-clean-up-buffers-too t)
    (setq dired-omit-files (rx bol (or (+ ".")
                                       (and "__pycache__" eol))))))

(use-package dired+
  :straight t
  :after dired
  :hook (dired-mode . dired-hide-details-mode))

(use-package wdired
  :after dired
  :preface
  (autoload 'evil-first-non-blank "evil-commands")
  :config
  (progn
    (evil-define-key 'normal wdired-mode-map (kbd "^") #'evil-first-non-blank)
    (evil-define-key 'normal dired-mode-map (kbd "C-c C-e") #'wdired-change-to-wdired-mode)))

(provide 'config-dired)

;;; config-dired.el ends here
