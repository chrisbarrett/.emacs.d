;;; config-themes.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'f)
(require 'general)
(require 'straight)



(defvar config-themes--dark-mode-p t)

(defun config-themes/toggle-dark-mode ()
  "Toggle between light and dark mode."
  (interactive)
  (if config-themes--dark-mode-p
      (config-themes/light-theme)
    (config-themes/dark-theme))
  (setq config-themes--dark-mode-p (not config-themes--dark-mode-p)))

(defun config-themes/light-theme ()
  "Load the light theme."
  (interactive)
  (load-theme 'cb-light t))

(defun config-themes/dark-theme ()
  "Load the dark theme."
  (interactive)
  (load-theme 'cb-dark t))



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
  :if (display-graphic-p)
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

(use-package ligatures
  :if (display-graphic-p)
  :hook
  ((prog-mode . ligatures-init)
   (text-mode . ligatures-init)
   (org-agenda-mode . ligatures-init))
  :config
  (global-prettify-symbols-mode +1))

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
  (defun cb-highlight-todo--enable-unless-org-buffer ()
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
    (add-hook 'text-mode-hook #'cb-highlight-todo--enable-unless-org-buffer)))

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
                    (makefile-mode all-the-icons-fileicon "gnu" :face all-the-icons-dorange)
                    (makefile-bsdmake-mode all-the-icons-fileicon "gnu" :face all-the-icons-dorange)
                    (gfm-mode all-the-icons-faicon "github-alt" :v-adjust 0.0 :face all-the-icons-lblue)
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
  (defun cb-leader-keys-set-up-hydra-buffer (&rest _)
    (when-let* ((buf (get-buffer " *LV*")))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local mode-line-format nil)
          (setq-local header-line-format nil)
          (force-mode-line-update)))))
  :config
  (advice-add 'lv-window :after #'cb-leader-keys-set-up-hydra-buffer))

(use-package pretty-hydra
  :straight (:host github :repo "chrisbarrett/major-mode-hydra.el")
  :preface
  (defun config-themes--head-formatting-function (key &optional hint)
    (cond
     ((char-or-string-p hint) ;; string hint
      (list (format " _%s_: %s" key hint)))
     ((or (null hint) (symbolp hint)) ;; no hint, doesn't show it in docstring at all
      nil)
     (t
      (list (format " _%s_: ?%s?" key key)))))
  :config
  (setq pretty-hydra-head-formatting-function #'config-themes--head-formatting-function))

(use-package imenu-list
  :straight t
  :commands (imenu-list)
  :init (require 'imenu-list-hacks)
  :general (:states 'normal :keymaps 'imenu-list-major-mode-map "q" #'quit-window))



;; Display a winsome pusheen gif in the scratch buffer during startup.

(eval-when-compile
  (require 'paths)
  (defvar config-themes--pusheen (create-image (f-join paths-assets-directory "pusheenicorn.gif") 'gif)))

(defun config-themes-display-pusheen ()
  (with-current-buffer "*scratch*"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "\n\n%13s" (propertize " " 'display config-themes--pusheen)))
      (read-only-mode +1))))

(defun config-themes-animate-pusheen ()
  (run-with-timer 0.01 nil (lambda ()
                             (let ((seconds 10))
                               (image-animate config-themes--pusheen nil seconds)))))

(config-themes-display-pusheen)
(add-hook 'after-init-hook #'config-themes-animate-pusheen)

(provide 'config-themes)

;;; config-themes.el ends here
