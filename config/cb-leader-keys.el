;;; cb-leader-keys.el --- Grab-bag for configuring general prefixed keys.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'subr-x)

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

(require 'hydra)

(autoload 'cb-basic-settings--echo-input-method-cleared "cb-basic-settings")
(autoload 'evil-window-rotate-downwards "evil-commands")
(autoload 'cb/alternate-buffer "cb-alternate-buffer")
(autoload 'cb/copy-buffer-name "cb-copy-buffer-path")
(autoload 'cb/copy-buffer-path "cb-copy-buffer-path")
(autoload 'cb/copy-buffer-directory "cb-copy-buffer-path")
(autoload 'cb/rename-file-and-buffer "cb-rename-file-and-buffer")
(autoload 'cb/sudo-edit "cb-sudo-edit")
(autoload 'cb/toggle-window-split "cb-toggle-window-split")
(autoload 'cb-goto-init-file "cb-goto")
(autoload 'cb-goto-messages "cb-goto")
(autoload 'cb-goto-nix-packages "cb-goto")
(autoload 'cb-goto-personal-config "cb-goto")
(autoload 'org-narrow-to-subtree "org")

(defhydra cb/select-input-method (:color blue :help nil)
  "Select input method"
  ("a" (progn (set-input-method "arabic") (message "Arabic input method activated")) "arabic")
  ("t" (progn (set-input-method "TeX") (message "TeX input method activated")) "TeX")
  ("SPC" (progn (deactivate-input-method) (message "Input method cleared")) "clear"))

(use-package cb-delete-current-buffer-and-file
  :commands (cb/delete-current-buffer-and-file)
  :preface
  (progn
    (autoload 'projectile-invalidate-cache "projectile")
    (autoload 'projectile-project-p "projectile")

    (defun cb-leader-keys--invalidate-cache (_path)
      (when (and (featurep 'projectile) (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache))))

  :config
  (add-hook 'cb-delete-current-buffer-and-file-functions #'cb-leader-keys--invalidate-cache))

(use-package which-key
  :straight t
  :preface
  (progn
    (autoload 'which-key-mode "which-key")
    (autoload 'which-key-add-key-based-replacements "which-key")

    (defun cb-leader-keys-set-up-which-key-buffer (&rest _)
      (when-let* ((buf (get-buffer which-key-buffer-name)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local mode-line-format nil)
            (setq-local header-line-format nil)
            (force-mode-line-update))))))

  :config
  (progn
    (setq which-key-special-keys nil)
    (setq which-key-use-C-h-commands t)
    (setq which-key-echo-keystrokes 0.02)
    (setq which-key-max-description-length 32)
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (setq which-key-idle-delay 0.4)
    (setq which-key-allow-evil-operators t)

    (advice-add 'which-key--create-buffer-and-show
                :after #'cb-leader-keys-set-up-which-key-buffer)

    ;; Strip cb prefixes from commands shown in which-key.

    (push `((nil . ,(rx bos "cb" (*? nonl) "/" (group (+? nonl))
                        (? "/body") eos))
            .
            (nil . "\\1"))
          which-key-replacement-alist)

    ;; Strip hydra body suffixes

    ;; Clean up comments entries

    (push `(("SPC c" . ,(rx (? "cb-evil-nerd-commenter/") (? "quick-") "comment-or-uncomment-" (group (+? nonl)))) . (nil . "\\1\\2"))
          which-key-replacement-alist)

    ;; Clean up errors entries

    (push `(("SPC e" . ,(rx (? "cb-") "flycheck-" (group (+? nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up goto and git

    (push `(("SPC g" . ,(rx (? "cb-") "magit-" (group (+? nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC g" . ,(rx "cb-" (group "goto-" (+? nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC g" . "time-machine-transient-state/body") . (nil . "git-time-machine"))
          which-key-replacement-alist)

    ;; Clean up help

    (push `(("SPC h d" . ,(rx bos (? "counsel-") "describe-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC h f" . ,(rx bos "find-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up navigation

    (push `(("SPC j" . ,(rx bos (? "evil-") "avy-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up kill

    (push `(("SPC k" . "kill-this-buffer") . (nil . "buffer"))
          which-key-replacement-alist)

    (push `(("SPC k" . "delete-window") . (nil . "window"))
          which-key-replacement-alist)

    (push `(("SPC k" . "counsel-yank-pop") . (nil . "kill-ring"))
          which-key-replacement-alist)

    ;; Clean up narrowing

    (push `(("SPC n" . ,(rx bos (? "org-") "narrow-to-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up org

    (push `(("SPC o" . ,(rx bos (? "cb-") (or "org-" "ledger-") (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up projectile

    (push `((nil . ,(rx bos (? "cb-") (? "counsel-") "projectile-" (group (+? nonl)) (? "-project") eos)) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `((nil . "projectile-dired") . (nil . "root (dired)"))
          which-key-replacement-alist)

    (push `((nil . "cb-neotree-find-project-root") . (nil . "root (neotree)"))
          which-key-replacement-alist)

    (push `(("SPC p" . ,(rx bos (*? nonl) "shell-command" (* nonl))) . (nil . "shell-command"))
          which-key-replacement-alist)

    (push `(("SPC p" . ,(rx bos (*? nonl) "async-shell-command" (* nonl))) . (nil . "shell-command (async)"))
          which-key-replacement-alist)

    ;; Clean up symbols

    (push `(("SPC s" . "evil-iedit-state/iedit-mode") . (nil . "iedit"))
          which-key-replacement-alist)

    ;; Clean up toggles

    (push `(("SPC t" . ,(rx bos "cb-" (? "faces/") (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up windows

    (push `(("SPC w" . ,(rx bos (? "cb-") (? "evil-") "window-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC w" . "balance-windows") . (nil . "balance"))
          which-key-replacement-alist)

    (push `(("SPC w" . "delete-window") . (nil . "delete"))
          which-key-replacement-alist)

    (push `(("SPC w" . "delete-other-windows") . (nil . "delete-others"))
          which-key-replacement-alist)

    ;; Clean up links

    (push `(("SPC x" . ,(rx bos "link-hint-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Clean up yasnippet

    (push `(("SPC y" . ,(rx bos (? "cb-") "yas" (any "-/") (group (+? nonl)) "-snippet" eos)) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `(("SPC y" . "yas-visit-snippet-file") . (nil . "visit-file"))
          which-key-replacement-alist)

    ;; Clean up transient states

    (push `((nil . ,(rx bos (group (+? nonl)) "-transient-state/body" eos)) . (nil . "\\1"))
          which-key-replacement-alist)

    ;; Add basic prefixes

    (which-key-add-key-based-replacements
      "SPC ,"   "smartparens"
      "SPC a"   "applications"
      "SPC b"   "buffers"
      "SPC c"   "comments"
      "SPC e"   "errors"
      "SPC f"   "files"
      "SPC g"   "git/goto"
      "SPC h"   "help"
      "SPC h d" "describe"
      "SPC h f" "find"
      "SPC k"   "kill"
      "SPC n"   "narrow"
      "SPC o"   "org"
      "SPC p"   "project"
      "SPC w"   "window"
      "SPC s"   "symbols"
      "SPC t"   "toggles"
      "SPC SPC" "M-x"
      "SPC m"   '("major-mode-cmd" . "Major mode commands"))

    (which-key-mode +1)))

(use-package spacemacs-keys
  :preface
  (progn
    (autoload 'evil-window-next "evil-commands")
    (autoload 'evil-window-split "evil-commands")
    (autoload 'evil-window-vsplit "evil-commands")

    (defun cb-leader-keys/reload-file ()
      "Revisit the current file."
      (interactive)
      (when-let* ((path (buffer-file-name)))
        (find-alternate-file path))))

  :config
  (progn
    (define-key universal-argument-map (kbd (concat "SPC u")) #'universal-argument-more)

    (spacemacs-keys-set-leader-keys
      "u"   #'universal-argument
      "SPC" #'execute-extended-command
      "TAB" #'cb/alternate-buffer
      "|"   #'cb/toggle-window-split
      ":"   #'eval-expression

      "!"   #'shell-command

      "a i" #'cb/select-input-method/body

      "b d" #'kill-this-buffer
      "b b" #'bury-buffer
      "b v" #'cb-leader-keys/reload-file

      "C" #'compile

      "c r" #'comment-or-uncomment-region

      "f d" #'cb/copy-buffer-directory
      "f D" #'cb/delete-current-buffer-and-file
      "f R" #'cb/rename-file-and-buffer
      "f e" #'cb/sudo-edit
      "f f" #'find-file
      "f o" #'find-file-other-window
      "f p" #'find-file-at-point
      "f s" #'save-buffer
      "f S" #'save-some-buffers
      "f W" #'write-file
      "f v" #'cb-leader-keys/reload-file
      "f y" #'cb/copy-buffer-path
      "f Y" #'cb/copy-buffer-name

      "g i" #'cb-goto-init-file
      "g m" #'cb-goto-messages
      "g n" #'cb-goto-nix-packages
      "g p" #'cb-goto-personal-config

      "h d c" #'describe-face
      "h d k" #'describe-key
      "h d m" #'describe-mode
      "h d p" #'describe-text-properties
      "h f c" #'find-face-definition
      "h f f" #'find-function
      "h f l" #'find-library
      "h f v" #'find-variable
      "h i"   #'info

      "k b" #'kill-this-buffer
      "k w" #'delete-window

      "n d" #'narrow-to-defun
      "n f" #'narrow-to-defun
      "n r" #'narrow-to-region
      "n s" #'org-narrow-to-subtree
      "n w" #'widen

      "q" #'delete-window

      "w =" #'balance-windows
      "w w" #'evil-window-next
      "w o" #'delete-other-windows
      "w q" #'delete-window
      "w r" #'evil-window-rotate-downwards
      "w -" #'evil-window-split
      "w /" #'evil-window-vsplit)))

(use-package cb-scale-font-transient-state
  :commands (cb-scale-font-transient-state/body)
  :init
  (progn
    (push '((nil . "cb-scale-font-transient-state/body") . (nil . "text-scale"))
          which-key-replacement-alist)

    (spacemacs-keys-set-leader-keys
      "z" #'cb-scale-font-transient-state/body)))

(use-package cb-buffer-transient-state
  :commands (cb-buffer-transient-state/body
             cb-buffer-transient-state/next-buffer
             cb-buffer-transient-state/previous-buffer)
  :init
  (spacemacs-keys-set-leader-keys
    "bn" #'cb-buffer-transient-state/next-buffer
    "bN" #'cb-buffer-transient-state/previous-buffer
    "bp" #'cb-buffer-transient-state/previous-buffer))

(provide 'cb-leader-keys)

;;; cb-leader-keys.el ends here
