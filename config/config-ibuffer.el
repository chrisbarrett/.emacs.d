;;; config-ibuffer.el --- Configuration for ibuffer  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'config-hydras)



;; ibuffer provides an interactive buffer list.

(use-package ibuffer
  :commands (ibuffer ibuffer-forward-line ibuffer-backward-line)
  :defines (ibuffer-show-empty-filter-groups
            ibuffer-never-show-predicates)
  :general ("C-x C-b" #'ibuffer-other-window)
  :general (:keymaps 'ibuffer-mode-map :states 'motion
                     "j" #'ibuffer-forward-line
                     "k" #'ibuffer-backward-line)
  :config
  (progn
    (general-setq
     ibuffer-expert t
     ibuffer-show-empty-filter-groups nil
     ibuffer-never-show-predicates
     (list (rx (or "*Messages*"
                   "*magit-"
                   "*git-auto-push*"
                   "*Backtrace*"
                   "*new*"
                   "*mu4e"
                   "*Org"
                   "*Flycheck error messages*"
                   "*Help*"))))

    (add-hook 'ibuffer-mode-hook #'hl-line-mode)

    (config-hydras-insinuate ibuffer-mode-map)))

;; ibuf-ext adds a few extra features to ibuffer.

(use-package ibuf-ext
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

;; ibuffer-projectile teaches ibuffer how to group files by projectile project.

(use-package ibuffer-projectile
  :straight t
  :commands (ibuffer-projectile-set-filter-groups)
  :functions (ibuffer-do-sort-by-alphabetic)
  :preface
  (defun config-ibuffer--setup-buffer ()
    (ibuffer-projectile-set-filter-groups)
    (add-to-list 'ibuffer-filter-groups '("Dired" (mode . dired-mode)))
    (add-to-list 'ibuffer-filter-groups '("Ensime" (predicate . (s-matches? "Ensime" (buffer-name)))))
    (add-to-list 'ibuffer-filter-groups '("System" (predicate . (-contains? '("*Messages*" "*scratch*") (buffer-name)))))
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :init
  (add-hook 'ibuffer-hook #'config-ibuffer--setup-buffer))

(provide 'config-ibuffer)

;;; config-ibuffer.el ends here
