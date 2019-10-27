;;; config-themes.el --- Typeface and syntax highlighting config.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'parameters)

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
            ibuffer-mode
            text-mode
            ledger-report-mode
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
    (add-to-list 'paren-face-modes 'typescript-mode)
    (add-to-list 'paren-face-modes 'rust-mode)
    (add-to-list 'paren-face-modes 'yaml-mode)

    (setq paren-face-regexp (rx (any "{}();,")))

    (font-lock-add-keywords 'js-mode
                    `((,(rx (any ":")) 0 'parenthesis)))

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
                   "PATCH"
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
                    (debugger-mode all-the-icons-faicon "stack-overflow" :v-adjust 0.05 :face all-the-icons-pink)))
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
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-major-mode-icon nil)
           (doom-modeline-buffer-encoding nil)
           (doom-modeline-enable-word-count t))
  :preface
  (defun config-themes--update-all-modelines ()
    "Ensure we update the header line in addition to the mode-line."
    (force-mode-line-update t))

  :config
  (progn
    (defun config-themes--right-top-window-p ()
      (--find (and (frame-parameter it 'fullscreen)
                   (or (equal 1 (length (window-list it 'never)))
                       (and (window-at-side-p (selected-window) 'right)
                            (window-at-side-p (selected-window) 'top))))
              (frame-list)))

    (setq org-clock-clocked-in-display nil)
    (add-hook 'org-clock-out-hook #'config-themes--update-all-modelines)
    (add-hook 'org-clock-in-hook #'config-themes--update-all-modelines)
    (add-hook 'org-clock-cancel-hook #'config-themes--update-all-modelines)

    (doom-modeline-def-segment org-clock
      (when (and (fboundp 'org-clocking-p) (org-clocking-p) (config-themes--right-top-window-p))
        (propertize (concat (org-clock-get-clock-string)
                            (doom-modeline-spc))
                    'face 'org-agenda-clocking)))

    (doom-modeline-def-segment space
      (doom-modeline-spc))

    (doom-modeline-def-segment battery
      "Display battery status."
      (when (and (bound-and-true-p display-battery-mode)
                 (config-themes--right-top-window-p))
        (or doom-modeline--battery-status (doom-modeline-update-battery-status))))

    (doom-modeline-def-segment system
      "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
      (when (config-themes--right-top-window-p)
        (let ((time (string-join (-map (-compose #'string-trim #'format-time-string)
                                       '("%a" "%e" "%b %R")) " ")))
          (propertize (concat (doom-modeline-spc) time (doom-modeline-spc))
                      'face `(:background ,(face-background 'region))))))

    ;; KLUDGE: Prevent the battery status from showing up in weird places.
    (defalias #'doom-modeline-segment--fancy-battery #'ignore)

    ;; override default modeline
    (doom-modeline-def-modeline 'main
      '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
      '(objed-state misc-info persp-name grip irc mu4e github debug lsp minor-modes input-method indent-info buffer-encoding
                    ;; major-mode
                    process vcs
                    checker
                    space
                    org-clock
                    battery
                    system))))

;; `fancy-battery' adds a mode for showing a battery indicator in the modeline.

(use-package fancy-battery
  :straight t
  :custom ((fancy-battery-show-percentage nil)
           (fancy-battery-mode-line nil))
  :hook ((after-init . fancy-battery-mode)
         (after-init . display-battery-mode)))

;; `hide-mode-line' provides a mode that hides the modeline.

(use-package hide-mode-line
  :straight t
  :hook (doom-modeline-mode . global-hide-mode-line-mode)
  :custom ((hide-mode-line-excluded-modes nil)))

;; doom-themes provides the main doom theme sets.

(use-package doom-themes
  :straight t
  :defer t
  :hook (after-init . (lambda () (require 'doom-themes)))
  :init
  (general-setq doom-themes-enable-bold t
                doom-themes-enable-italic t
                doom-treemacs-enable-variable-pitch nil)
  :init
  (defface posframe-border
    '((t ()))
    "Face for posframe borders."
    :group 'config-themes)

  :preface
  (progn
    (defun config-themes-toggle (&optional theme)
      (interactive)
      (let* ((current-theme (car custom-enabled-themes))
             (new-theme (or theme (if (equal 'doom-one current-theme)
                                      'doom-solarized-light
                                    'doom-one))))
        (enable-theme new-theme)
        (config-themes-override-themes new-theme))
      ;; HACK: Make sure bullets are re-fontified.
      (when (bound-and-true-p org-bullets-mode)
        (font-lock-fontify-buffer)))

    (defun config-themes-override-themes (theme)
      (let* ((bg         (pcase theme
                           ('doom-solarized-light "#FDF6E3")
                           ('doom-one "#282c34")))
             (bg-alt     (pcase theme
                           ('doom-solarized-light "#FFFBEA")
                           ('doom-one "#21242b")))
             (base0      (pcase theme
                           ('doom-solarized-light "#FFFBF0")
                           ('doom-one "#1B2229")))
             (base1      (pcase theme
                           ('doom-solarized-light "#FCF8ED")
                           ('doom-one "#1c1f24")))
             (base2      (pcase theme
                           ('doom-solarized-light "#FCF7E8")
                           ('doom-one "#202328")))
             (base3      (pcase theme
                           ('doom-solarized-light "#F2E6CE")
                           ('doom-one "#23272e")))
             (base4      (pcase theme
                           ('doom-solarized-light "#E1DBCD")
                           ('doom-one "#3f444a")))
             (base5      (pcase theme
                           ('doom-solarized-light "#D6D6D6")
                           ('doom-one "#5B6268")))
             (base6      (pcase theme
                           ('doom-solarized-light "#96A7A9")
                           ('doom-one "#73797e")))
             (base7      (pcase theme
                           ('doom-solarized-light "#788484")
                           ('doom-one "#9ca0a4")))
             (base8      (pcase theme
                           ('doom-solarized-light "#626C6C")
                           ('doom-one "#DFDFDF")))
             (fg         (pcase theme
                           ('doom-solarized-light "#556b72")
                           ('doom-one "#bbc2cf")))
             (fg-alt     (pcase theme
                           ('doom-solarized-light "#7B8787")
                           ('doom-one "#5B6268")))
             (bg-alt-l   (pcase theme
                           ('doom-solarized-light (doom-darken bg 0.05))
                           ('doom-one bg-alt)))
             (orange     (pcase theme
                           ('doom-solarized-light "#cb4b16")
                           ('doom-one "#da8548")))
             (red       (pcase theme
                          ('doom-solarized-light "#dc322f")
                          ('doom-one "#ff6c6b")))
             (blue       (pcase theme
                           ('doom-solarized-light "#268bd2")
                           ('doom-one "#51afef"))))

        (set-face-attribute 'default nil :family parameters-default-font-family)
        (set-face-attribute 'default nil :height parameters-default-text-height)
        (set-face-attribute 'font-lock-keyword-face nil :weight 'light)
        (set-face-attribute 'font-lock-string-face nil :weight 'light)

        (apply 'custom-theme-set-faces theme
               (append

                ;; Common settings

                `((font-lock-comment-face ((t (:weight bold))))
                  (org-agenda-current-time ((t :foreground ,orange)))
                  (ledger-report-negative-amount ((t :foreground ,red)))
                  (vterm-color-black ((t (:foreground ,bg :background ,base5))))
                  (org-agenda-clocking ((t (:bold nil :foreground ,fg :background ,(doom-blend blue bg 0.2)))))
                  (ivy-posframe-border ((t (:inherit posframe-border))))
                  (hydra-posframe-face ((t (:background ,bg-alt-l))))
                  (hydra-posframe-border-face ((t (:inherit posframe-border))))
                  (doom-modeline-project-dir ((t (:inherit font-lock-string-face :weight normal :bold t))))
                  (org-funcs-agenda-note ((t (:inherit font-lock-string-face :italic t))))
                  (markdown-list-face ((t :inherit markdown-blockquote-face)))
                  (treemacs-git-unmodified-face ((t :inherit default)))
                  (treemacs-git-renamed-face ((t :inherit font-lock-doc-face)))
                  (org-drawer ((t :inherit org-special-keyword)))
                  (outline-1 ((t (:weight bold :foreground ,blue))))
                  (ledger-occur-xact-face ((t :inherit region)))
                  (lsp-ui-sideline-symbol ((t :height 0.99)))
                  (lsp-ui-sideline-symbol-info ((t :foreground "grey" :slant italic :height 0.99 :weight light)))
                  (lsp-ui-sideline-current-symbol ((t (:inherit lsp-face-highlight-read :height 0.99))))

                  ;; we need to set ivy colours to dark manually.
                  (ivy-minibuffer-match-face-1 ((t (:foreground ,bg :background ,base7 :weight bold))))
                  (ivy-minibuffer-match-face-2 ((t (:foreground ,bg :background ,base6 :weight bold))))
                  (ivy-minibuffer-match-face-3 ((t (:foreground ,fg :background ,base4 :weight bold))))
                  (ivy-minibuffer-match-face-4 ((t (:foreground ,bg :background ,base8)))))

                ;; Theme-specific settings.

                (pcase theme
                  ('doom-one
                   `((doom-modeline-project-parent-dir ((t (:foreground ,base5 :bold t))))
                     (org-scheduled ((t (:foreground ,base4))))
                     (org-scheduled-today ((t (:foreground ,fg))))
                     (treemacs-git-ignored-face ((t :foreground "#787878")))
                     (parenthesis ((t (:foreground "#787878" :weight light))))
                     ;; HACK: This doesn't seem to get set properly in the face definition.
                     (org-block-end-line ((t :foreground ,base5 :background ,base3)))))

                  ('doom-solarized-light
                   `((doom-modeline-project-parent-dir ((t (:foreground ,base6 :bold t))))
                     (parenthesis ((t (:foreground "#9c9c9c" :weight light))))))))))))

  :config
  (progn
    (doom-themes-treemacs-config)
    (doom-themes-org-config)

    ;; Customise themes.

    (dolist (theme '(doom-one doom-solarized-light))
      (load-theme theme t t))

    ;; Enable theme.

    (config-themes-toggle parameters-default-theme)))


(defun config-themes--after-enable-theme (&rest _)
  ;; Delete posframes after changing themes.
  (when (fboundp 'posframe-delete-all)
    (posframe-delete-all))
  ;; Force org buffers to refontify to fix org-bullet properties.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (font-lock-flush (point-min) (point-max))))))

(advice-add 'enable-theme :after #'config-themes--after-enable-theme)

(provide 'config-themes)

;;; config-themes.el ends here
