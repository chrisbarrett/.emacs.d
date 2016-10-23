;;; cb-evil.el --- Configuration for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'evil-transient-state)

(use-package evil
  :preface
  (defun cb-evil--sp-delete-and-join-compat (fn &rest args)
    (if (bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char)
      (apply fn args)))

  :config
  (progn
    (evil-mode +1)
    (setq-default evil-shift-width 2)

    (setq evil-want-Y-yank-to-eol t)
    (setq evil-symbol-word-search t)

    ;; Configure cursors.

    (setq evil-motion-state-cursor '("plum3" box))
    (setq evil-visual-state-cursor '("gray" (hbar . 2)))
    (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
    (setq evil-emacs-state-cursor  '("SkyBlue2" hbar))

    ;; Motion keys for help buffers.

    (evil-define-key 'motion help-mode-map (kbd "<escape>") #'quit-window)
    (evil-define-key 'motion help-mode-map (kbd "<tab>") #'forward-button)
    (evil-define-key 'motion help-mode-map (kbd "S-<tab>") #'backward-button)
    (evil-define-key 'motion help-mode-map (kbd "]") #'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "gf") #'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "[") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gb") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gh") #'help-follow-symbol)


    ;; Better compat with smartparens-strict mode.
    ;; TODO: Move to SP config.

    (advice-add #'evil-delete-backward-char-and-join
                :around #'cb-evil--sp-delete-and-join-compat))

  :functions (evil-mode evil-delay evil-delete-backward-char-and-join)
  :defines (evil-want-Y-yank-to-eol))

(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :commands (evil-terminal-cursor-changer-activate)
  :config (evil-terminal-cursor-changer-activate))

(use-package evil-surround
  :after evil
  :commands (global-evil-surround-mode
             evil-surround-region)

  :preface
  (autoload 'evil-substitute "evil-commands")

  :config
  (progn
    (setq-default evil-surround-pairs-alist
                  '((?\( . ("(" . ")"))
                    (?\[ . ("[" . "]"))
                    (?\{ . ("{" . "}"))

                    (?\) . ("(" . ")"))
                    (?\] . ("[" . "]"))
                    (?\} . ("{" . "}"))

                    (?# . ("#{" . "}"))
                    (?b . ("(" . ")"))
                    (?B . ("{" . "}"))
                    (?> . ("<" . ">"))
                    (?t . surround-read-tag)
                    (?< . surround-read-tag)
                    (?f . surround-function)))

    (global-evil-surround-mode)
    (evil-define-key 'visual evil-surround-mode-map "s" #'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" #'evil-substitute)))


(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (spacemacs-keys-set-leader-keys "se" #'evil-iedit-state/iedit-mode)

  :config
  (progn
    (setq iedit-current-symbol-default t)
    (setq iedit-only-at-symbol-boundaries t)
    (setq iedit-toggle-key-default nil)

    ;; Enable leader key in iedit and iedit-insert states
    (define-key evil-iedit-state-map (kbd "SPC") spacemacs-keys-default-map)))

(use-package evil-ediff
  :after ediff)

(use-package evil-args
  :after evil
  :config
  (progn
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package evil-indent-plus
  :after evil
  :commands (evil-indent-plus-default-bindings)
  :config (evil-indent-plus-default-bindings))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator)
  :preface
  (require 'cb-evil-nerd-commenter)
  :init
  (progn
    ;; Double all the commenting functions so that the inverse operations
    ;; can be called without setting a flag
    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
    (define-key evil-normal-state-map "gy" 'spacemacs/copy-and-comment-lines)

    (spacemacs-keys-set-leader-keys
      ";"  #'evilnc-comment-operator
      "cl" #'cb-evil-nerd-commenter/comment-or-uncomment-lines
      "cL" #'cb-evil-nerd-commenter/comment-or-uncomment-lines-inverse
      "cp" #'cb-evil-nerd-commenter/comment-or-uncomment-paragraphs
      "cP" #'cb-evil-nerd-commenter/comment-or-uncomment-paragraphs-inverse
      "ct" #'cb-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line
      "cT" #'cb-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line-inverse
      "cy" #'cb-evil-nerd-commenter/copy-and-comment-lines
      "cY" #'cb-evil-nerd-commenter/copy-and-comment-lines-inverse)))

(use-package evil-matchit
  :after evil)

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt)

  :preface
  (evil-transient-state-define evil-numbers
    :title "Evil Numbers Transient State"
    :doc "\n[_+_/_=_] increase number  [_-_] decrease  [0..9] prefix  [_q_] quit"
    :bindings
    ("+" evil-numbers/inc-at-pt)
    ("=" evil-numbers/inc-at-pt)
    ("-" evil-numbers/dec-at-pt)
    ("q" nil :exit t))

  :init
  (progn
    (spacemacs-keys-declare-prefix "n" "numbers")
    (spacemacs-keys-set-leader-keys
      "n+" #'evil-numbers-transient-state/evil-numbers/inc-at-pt
      "n=" #'evil-numbers-transient-state/evil-numbers/inc-at-pt
      "n-" #'evil-numbers-transient-state/evil-numbers/dec-at-pt)))

(use-package evil-search-highlight-persist
  :after evil
  :commands (global-evil-search-highlight-persist
             evil-search-highlight-persist-remove-all)

  :preface
  (autoload 'evil-ex-define-cmd "evil-ex")

  :config
  (progn
    (global-evil-search-highlight-persist)
    (evil-ex-define-cmd "noh" #'evil-search-highlight-persist-remove-all)))

(use-package evil-visual-mark-mode
  :after evil
  :commands (evil-visual-mark-mode)
  :config (evil-visual-mark-mode))

(use-package vi-tilde-fringe
  :after evil
  :commands (vi-tilde-fringe-mode global-vi-tilde-fringe-mode)

  :preface
  (progn
    (defun cb-evil--vi-tilde-fringe-off ()
      (vi-tilde-fringe-mode -1))

    (defun cb-evil--vi-tilde-fringe-off-if-readonly ()
      (when buffer-read-only
        (vi-tilde-fringe-mode -1))))

  :config
  (progn
    (add-hook 'which-key-init-buffer-hook #'cb-evil--vi-tilde-fringe-off)
    (add-hook 'after-change-major-mode-hook #'cb-evil--vi-tilde-fringe-off-if-readonly)
    (global-vi-tilde-fringe-mode)))


(provide 'cb-evil)

;;; cb-evil.el ends here
