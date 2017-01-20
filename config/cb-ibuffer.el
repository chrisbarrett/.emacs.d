;;; cb-ibuffer.el --- Configuration for ibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'evilified-state)

(use-package ibuffer
  :commands (ibuffer ibuffer-forward-line ibuffer-backward-line)
  :defines (ibuffer-show-empty-filter-groups
            ibuffer-never-show-predicates)
  :bind ("C-x C-b" . ibuffer-other-window)
  :init
  (spacemacs-keys-set-leader-keys "b l" #'ibuffer)

  :preface
  ;; HACK: Hide the cursor and use hl-line.
  (defun cb-ibuffer--hacky-show-line-only ()
    (run-with-timer 0.01 nil (lambda ()
                               (setq cursor-type nil)
                               (hl-line-mode +1))))

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
    (add-hook 'ibuffer-hook #'cb-ibuffer--hacky-show-line-only t)
    (define-key ibuffer-mode-map (kbd "SPC") spacemacs-keys-default-map)
    (define-key ibuffer-mode-map (kbd "j") #'ibuffer-forward-line)
    (define-key ibuffer-mode-map (kbd "k") #'ibuffer-backward-line)))

(use-package ibuf-ext
  :commands (ibuffer-auto-mode)
  :init
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-projectile
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

(provide 'cb-ibuffer)

;;; cb-ibuffer.el ends here
