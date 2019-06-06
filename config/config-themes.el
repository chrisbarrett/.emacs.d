;;; config-themes.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'vars)

;; menu-bar, tool-bar and scroll-bar are builtin features that aren't very
;; useful in a keyboard-driven interface.

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

;; page-break-lines shows a horizontal separator in buffers instead of a
;; page-break control character (^L).

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

;; paren-face applies a separate face for parens, allowing them to be
;; de-emphasised.

(use-package paren-face
  :straight t
  :demand t
  :commands (global-paren-face-mode)
  :config
  (progn
    (set-face-attribute 'parenthesis nil
                        :inherit 'font-lock-comment-face
                        :weight 'light
                        :italic nil
                        :background nil)

    (add-to-list 'paren-face-modes 'scala-mode)
    (add-to-list 'paren-face-modes 'js-mode)
    (add-to-list 'paren-face-modes 'rust-mode)
    (add-to-list 'paren-face-modes 'yaml-mode)

    (setq paren-face-regexp (rx (any "{}():;,")))

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

;; hl-todo applies a separate face for todo keywords so they can be highlighted.

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

;; all-the-icons provides a set of icons that can be rendered by Emacs.

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
                    (mu4e-main-mode all-the-icons-octicon "inbox" :face all-the-icons-dsilver)
                    (mu4e-headers-mode all-the-icons-octicon "inbox" :face all-the-icons-dsilver)
                    (mu4e-view-mode all-the-icons-octicon "comment-discussion" :face all-the-icons-dsilver)
                    (mu4e-compose-mode all-the-icons-octicon "comment-discussion" :face all-the-icons-orange)))
      (add-to-list 'all-the-icons-mode-icon-alist spec))))

;; hydra provides keyboard-driven UI elements.

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

;; imenu-list provides a list UI for imenu items.

(use-package imenu-list
  :straight t
  :commands (imenu-list)
  :general (:states 'normal :keymaps 'imenu-list-major-mode-map "q" #'quit-window))

;; which-key displays available key bindings under the current prefix.

(use-package which-key
  :straight t
  :defer t
  :hook (after-init . which-key-mode)
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
              ((nil . ,custom-regex) . (nil . "\\1")))))))

;; prettify-symbols-mode allows characters to be displayed in a different way.

(use-package prettify-symbols-mode
  :hook (prog-mode . prettify-symbols-mode)
  :preface
  (defun config-themes--set-up-prettify-symbols ()
    (cond
     ((derived-mode-p 'emacs-lisp-mode 'lisp-mode 'scheme-mode)
      (setq-local prettify-symbols-alist '(("lambda" . ?λ))))))
  :config
  (add-hook 'prettify-symbols-mode-hook #'config-themes--set-up-prettify-symbols))

;; emojify adds support for rendering emojis.

(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode)
  :config
  (general-setq emojify-emoji-styles '(github unicode)
                emojify-program-contexts '(comments string)
                emojify-point-entered-behaviour 'uncover))

;; doom-modeline is a custom modeline.

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

;; doom-themes provides the main doom theme sets.

(use-package doom-themes
  :straight t
  :init
  (general-setq doom-themes-enable-bold t
                doom-themes-enable-italic t
                doom-treemacs-enable-variable-pitch nil)
  :preface
  (defun config-themes-toggle (&optional theme)
    (interactive)
    (let* ((current-theme (car custom-enabled-themes))
           (new-theme (or theme (if (equal 'doom-one current-theme)
                                    'doom-solarized-light
                                  'doom-one))))
      (enable-theme new-theme)
      (pcase new-theme
        (`doom-solarized-light
         (let ((blue "#268bd2")
               (turquoise "#2aa198"))
           (custom-theme-set-faces
            'doom-solarized-light
            `(default ((t (:foreground "#556b72"
                           :background "#FDF6E3"
                           :height ,vars-default-text-height
                           :family ,vars-default-font-family))))
            `(font-lock-comment-face ((t (:weight bold))))
            `(font-lock-string-face ((t (:weight light :foreground ,turquoise))))
            `(font-lock-keyword-face ((t (:weight light))))
            `(parenthesis ((t (:foreground "#9c9c9c" :weight light))))
            `(outline-1 ((t (:weight bold :foreground ,blue))))
            `(lsp-ui-sideline-symbol ((t :height 0.99)))
            `(lsp-ui-sideline-symbol-info ((t :foreground "grey" :slant italic :height 0.99 :weight light)))
            `(lsp-ui-sideline-current-symbol ((t (:inherit lsp-face-highlight-read :height 0.99)))))))

        (`doom-one
         (let ((blue "#51afef")
               (green "#98be65"))
           (custom-theme-set-faces
            'doom-one
            `(default ((t (:foreground "#bbc2cf"
                           :background "#282c34"
                           :height ,vars-default-text-height
                           :family ,vars-default-font-family))))
            `(parenthesis ((t (:foreground "#787878" :weight light))))
            `(font-lock-comment-face ((t (:weight bold))))
            `(font-lock-string-face ((t (:weight light :foreground ,green))))
            `(font-lock-keyword-face ((t (:weight light))))
            `(outline-1 ((t (:weight bold :foreground ,blue))))
            ;; HACK: This doesn't seem to get set properly.
            `(org-block-end-line ((t :foreground "#5B6268" :background "#23272e")))
            `(lsp-ui-sideline-symbol ((t :height 0.99)))
            `(lsp-ui-sideline-symbol-info ((t :foreground "grey" :slant italic :height 0.99 :weight light)))
            `(lsp-ui-sideline-current-symbol ((t (:inherit lsp-face-highlight-read :height 0.99))))))))))

  :config
  (progn
    (load-theme 'doom-one t t)
    (load-theme 'doom-solarized-light t t)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (config-themes-toggle vars-default-theme)))

(provide 'config-themes)

;;; config-themes.el ends here
