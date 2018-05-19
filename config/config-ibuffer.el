;;; config-ibuffer.el --- Configuration for ibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'config-hydras)
(require 'evilified-state)

(use-package ibuffer
  :commands (ibuffer ibuffer-forward-line ibuffer-backward-line)
  :defines (ibuffer-show-empty-filter-groups
            ibuffer-never-show-predicates)
  :bind ("C-x C-b" . ibuffer-other-window)
  :config
  (progn
    (setq ibuffer-expert t)
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-never-show-predicates
          (list (rx (or "*Messages*"
                        "*magit-"
                        "*git-auto-push*"
                        "*Backtrace*"
                        "*new*"
                        "*mu4e"
                        "*Org"
                        "*Flycheck error messages*"
                        "*Help*"))))

    (config-hydras-insinuate ibuffer-mode-map)
    (define-key ibuffer-mode-map (kbd "j") #'ibuffer-forward-line)
    (define-key ibuffer-mode-map (kbd "k") #'ibuffer-backward-line)))

(use-package ibuf-ext
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-projectile
  :straight t
  :commands (ibuffer-projectile-set-filter-groups)
  :functions (ibuffer-do-sort-by-alphabetic)
  :preface
  (defun cb-ibuffer--setup-buffer ()
    (ibuffer-projectile-set-filter-groups)
    (add-to-list 'ibuffer-filter-groups '("Dired" (mode . dired-mode)))
    (add-to-list 'ibuffer-filter-groups '("Ensime" (predicate . (s-matches? "Ensime" (buffer-name)))))
    (add-to-list 'ibuffer-filter-groups '("System" (predicate . (-contains? '("*Messages*" "*scratch*") (buffer-name)))))
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :init
  (add-hook 'ibuffer-hook #'cb-ibuffer--setup-buffer))

(provide 'config-ibuffer)

;;; config-ibuffer.el ends here
