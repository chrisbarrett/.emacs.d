;;; config-themes.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package menu-bar
  :if (bound-and-true-p menu-bar-mode)
  :general ("C-c e e" #'toggle-debug-on-error)
  :config
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))


(use-package tool-bar
  :if (bound-and-true-p tool-bar-mode)
  :config
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)))


(use-package scroll-bar
  :if (bound-and-true-p scroll-bar-mode)
  :config
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)))


(use-package page-break-lines
  :straight t
  :commands (global-page-break-lines-mode)
  :demand t
  :config
  (progn
    (setq page-break-lines-modes
          '(prog-mode
            text-mode
            imenu-list-major-mode
            compilation-mode
            help-mode
            org-agenda-mode))

    (global-page-break-lines-mode)))


(use-package paren-face
  :straight t
  :demand t
  :commands (global-paren-face-mode)
  :config
  (progn
    (add-to-list 'paren-face-modes 'scala-mode)
    (add-to-list 'paren-face-modes 'web-mode)
    (add-to-list 'paren-face-modes 'rust-mode)
    (add-to-list 'paren-face-modes 'yaml-mode)
    (setq paren-face-regexp (rx (any "{}();,")))

    (font-lock-add-keywords 'rust-mode
                    `(;; Type assertions
                      (,(rx (any ":")) 0 'parenthesis)
                      ;; Generic type parameters
                      (,(rx (group "<") symbol-start) 1 'parenthesis)
                      (,(rx symbol-end (group (+ ">"))) 1 'parenthesis)
                      ;; Lambda parameter delimiters
                      (,(rx (group "|") (not (any "|"))) 1 'parenthesis)))

    (font-lock-add-keywords 'scala-mode
                    `(;; Type assertions
                      (,(rx (any ":")) 0 'parenthesis)
                      ;; Generic type parameters
                      (,(rx (group "[") symbol-start) 1 'parenthesis)
                      (,(rx symbol-end (group (+ "]"))) 1 'parenthesis)))

    (global-paren-face-mode +1)))


(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)

  :preface
  (defun config-themes--enable-hl-todo-unless-org-buffer ()
    (unless (derived-mode-p 'org-mode)
      (hl-todo-mode)))

  :init
  (progn
    (setq hl-todo-keyword-faces
          (--map (cons it 'hl-todo)
                 '("TODO"
                   "NEXT"
                   "HACK"
                   "FIXME"
                   "KLUDGE"
                   "NOTE")))
    (add-hook 'text-mode-hook #'config-themes--enable-hl-todo-unless-org-buffer)))


(use-package all-the-icons
  :straight t
  :defer t
  :init
  (defvar all-the-icons-scale-factor 1.0)
  :config
  (progn
    ;; HACK: Declare variable which is unsafely referenced inside lib.
    (defvar web-mode-content-type nil)
    (dolist (spec '((nix-mode all-the-icons-faicon "linux" :face all-the-icons-purple)
                    (ledger-mode all-the-icons-material "account_balance")
                    (makefile-mode all-the-icons-fileicon "gnu" :face all-the-icons-dorange)
                    (makefile-bsdmake-mode all-the-icons-fileicon "gnu" :face all-the-icons-dorange)
                    (gfm-mode all-the-icons-faicon "github-alt" :v-adjust 0.0 :face all-the-icons-lblue)
                    (helpful-mode all-the-icons-icon-for-mode help-mode)
                    (debugger-mode all-the-icons-faicon "stack-overflow" :v-adjust 0.05 :face all-the-icons-pink)
                    (web-js-mode all-the-icons-icon-for-mode web-mode :v-adjust 0.0 :face all-the-icons-lblue)
                    (web-ts-mode all-the-icons-icon-for-mode web-mode :v-adjust 0.0 :face all-the-icons-lblue)
                    (web-css-mode all-the-icons-icon-for-mode web-mode :v-adjust 0.0 :face all-the-icons-lblue)
                    (web-html-mode all-the-icons-icon-for-mode web-mode :v-adjust 0.0 :face all-the-icons-lblue)
                    (web-json-mode all-the-icons-icon-for-mode web-mode :v-adjust 0.0 :face all-the-icons-lblue)
                    (mu4e-main-mode all-the-icons-octicon "inbox" :face all-the-icons-dsilver)
                    (mu4e-headers-mode all-the-icons-octicon "inbox" :face all-the-icons-dsilver)
                    (mu4e-view-mode all-the-icons-octicon "comment-discussion" :face all-the-icons-dsilver)
                    (mu4e-compose-mode all-the-icons-octicon "comment-discussion" :face all-the-icons-orange)))
      (add-to-list 'all-the-icons-mode-icon-alist spec))))


(use-package hydra
  :straight t
  :preface
  (defun config-themes--set-up-hydra-buffer (&rest _)
    (when-let* ((buf (get-buffer " *LV*")))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local mode-line-format nil)
          (setq-local header-line-format nil)
          (force-mode-line-update)))))
  :config
  (advice-add 'lv-window :after #'config-themes--set-up-hydra-buffer))


(provide 'config-themes)

;;; config-themes.el ends here
