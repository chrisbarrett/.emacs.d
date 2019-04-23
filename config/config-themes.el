;;; config-themes.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'f)
(require 'general)
(require 'imenu-list-hacks)
(require 'straight)



(defvar config-themes--manual-theme nil)

(defun config-themes/toggle-dark-mode ()
  "Toggle between light and dark mode."
  (interactive)
  (setq config-themes--manual-theme t)
  (if (equal 'cb-dark (car custom-enabled-themes))
      (config-themes/light-theme)
    (config-themes/dark-theme)))

(defun config-themes/light-theme ()
  "Load the light theme."
  (interactive)
  (unless (equal 'cb-light (car custom-enabled-themes))
    (load-theme 'cb-light t)))

(defun config-themes/dark-theme ()
  "Load the dark theme."
  (interactive)
  (unless (equal 'cb-dark (car custom-enabled-themes))
    (load-theme 'cb-dark t)))

(defun config-themes-set-for-time-of-day ()
  (unless config-themes--manual-theme
    (-let [(_sec _min hour) (decode-time)]
      (if (< 8 hour 20)
          (config-themes/light-theme)
        (config-themes/dark-theme)))))



(use-package solarized-theme
  :straight t
  :defer t
  :init
  (general-setq
   ;; Put underline below descent for better legibility.
   x-underline-at-descent-line t

   solarized-use-less-bold t

   ;; Don't use variable-pitch.
   solarized-use-variable-pitch nil

   ;; Don't use variable scale text.
   solarized-scale-org-headlines nil
   solarized-height-minus-1 1.0
   solarized-height-plus-1 1.0
   solarized-height-plus-2 1.0
   solarized-height-plus-3 1.0
   solarized-height-plus-4 1.0))

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
  :straight (:host github
             :repo "chrisbarrett/major-mode-hydra.el"
             :fork (:host github
                    :repo "jerrypnz/major-mode-hydra.el"))
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
  :general (:states 'normal :keymaps 'imenu-list-major-mode-map "q" #'quit-window))

(use-package which-key
  :straight t
  :config
  (progn
    (setq which-key-idle-delay 0.4)
    (setq which-key-replacement-alist
          (let ((custom-regex
                 (rx bos
                     ;; strip hydra prefix
                     (? (and (+? nonl) "/"))
                     (? (or
                         ;; feature names
                         "counsel"
                         "evil")
                        "-")
                     (group (+ nonl)))))
            `((("<left>") . ("←"))
              (("<right>") . ("→"))
              (("<\\([[:alnum:]-]+\\)>") . ("\\1"))
              ((nil . "Prefix Command") . (nil . "prefix"))
              ((nil . "\\`\\?\\?\\'") . (nil . "lambda"))
              ((nil . "which-key-show-next-page-no-cycle") . (nil . "wk next pg"))
              ((nil . ,custom-regex) . (nil . "\\1")))))
    (which-key-mode)))

;; Display a winsome pusheen gif in the scratch buffer during startup.

(use-package pusheen
  :disabled t
  :config
  (progn
    (with-current-buffer "*scratch*"
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "\n\n%13s" (pusheen 'unicorn)))
        (read-only-mode +1)))

    (add-hook 'after-init-hook #'pusheen-animate-all)))

(provide 'config-themes)

;;; config-themes.el ends here
