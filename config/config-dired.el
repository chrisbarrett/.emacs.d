;;; config-dired.el --- Configuration for dired.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'spacemacs-keys)

(autoload 'evil-define-key "evil")

(spacemacs-keys-declare-prefix-for-mode 'dired-mode "ms" "subdir")



(use-package which-key
  :config
  (push `(dired-mode
          ((nil . ,(rx bos "dired-" (group (+ nonl)))) . (nil . "\\1")))
        which-key-replacement-alist))

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

  :init
  (spacemacs-keys-set-leader-keys-for-major-mode 'dired-mode
    "d"  'dired-hide-details-mode
    "si" 'dired-insert-subdir
    "sd" 'dired-kill-subdir)

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
  (progn
    (add-hook 'dired-load-hook (lambda () (load "dired-x")))
    (spacemacs-keys-set-leader-keys-for-major-mode 'dired-mode
      "h" #'dired-omit-mode))
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
