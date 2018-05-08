;;; cb-evil.el --- Configuration for evil-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package evil
  :straight t
  :preface
  (progn
    (autoload 'evil-set-initial-state "evil-core")
    (autoload 'evil-visual-update-x-selection "evil-states")

    (defun cb-evil--bounds-of-surrounding-lines (lines-before lines-after)
      (let ((start
             (save-excursion
               (ignore-errors
                 (forward-line (- lines-before)))
               (line-beginning-position)))
            (end
             (save-excursion
               (ignore-errors
                 (forward-line lines-after))
               (line-end-position))))
        (list start end)))

    (defun cb-evil--sp-delete-and-join-compat (fn &rest args)
      (cond
       ;; Narrow before deleting to improve performance in large org buffers.
       ((and (bound-and-true-p smartparens-strict-mode)
             (derived-mode-p 'org-mode))
        (save-restriction
          (apply #'narrow-to-region (cb-evil--bounds-of-surrounding-lines 10 10))
          (call-interactively 'sp-backward-delete-char)))

       ((bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char))

       (t
        (apply fn args)))))

  :config
  (progn
    (evil-mode +1)
    (setq evil-mode-line-format nil)
    (setq-default evil-shift-width 2)
    (setq-default evil-symbol-word-search t)

    (setq evil-want-visual-char-semi-exclusive t)
    (setq evil-want-Y-yank-to-eol t)

    ;; Prevent visual state from updating the clipboard.

    (advice-add #'evil-visual-update-x-selection :override #'ignore)

    ;; Configure cursors.

    (setq evil-motion-state-cursor '("plum3" box))
    (setq evil-visual-state-cursor '("gray" (hbar . 2)))
    (setq evil-normal-state-cursor '("IndianRed" box))
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
    (setq evil-emacs-state-cursor  '("SkyBlue2" hbar))

    ;; Motion keys for help buffers.

    (evil-define-key 'motion help-mode-map (kbd "<escape>") #'quit-window)
    (evil-define-key 'motion help-mode-map (kbd "<tab>") #'forward-button)
    (evil-define-key 'motion help-mode-map (kbd "S-<tab>") #'backward-button)
    (evil-define-key 'motion help-mode-map (kbd "]") #'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "gf") #'help-go-forward)
    (evil-define-key 'motion help-mode-map (kbd "^") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "[") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gb") #'help-go-back)
    (evil-define-key 'motion help-mode-map (kbd "gh") #'help-follow-symbol)

    ;; Make sure these stupid bindings don't sneak back in.

    (evil-global-set-key 'normal [remap redo] #'undo-tree-redo)
    (evil-global-set-key 'normal [remap undo] #'undo-tree-undo)

    ;; Initial states

    (with-eval-after-load 'replace
      (evil-set-initial-state 'occur-mode 'motion)
      (evil-add-hjkl-bindings occur-mode-map))

    (with-eval-after-load 'tar-mode
      (evil-set-initial-state 'tar-mode 'emacs)
      (evil-add-hjkl-bindings tar-mode-map))
    (with-eval-after-load 'arc-mode
      (evil-set-initial-state 'archive-mode 'emacs)
      (evil-add-hjkl-bindings archive-mode-map))

    (evil-set-initial-state 'wdired-mode 'normal)

    ;; Better compat with smartparens-strict mode.
    ;; TODO: Move to SP config.

    (advice-add #'evil-delete-backward-char-and-join
                :around #'cb-evil--sp-delete-and-join-compat))

  :functions (evil-mode evil-delay evil-delete-backward-char-and-join)
  :defines (evil-want-Y-yank-to-eol))

(use-package evil-terminal-cursor-changer
  :straight t
  :if (not (display-graphic-p))
  :commands (evil-terminal-cursor-changer-activate)
  :config (evil-terminal-cursor-changer-activate))

(use-package evil-surround
  :straight t
  :commands (global-evil-surround-mode
             evil-surround-region)

  :preface
  (progn
    (autoload 'evil-substitute "evil-commands")

    (defun cb-evil--init-evil-surround-pairs ()
      (make-local-variable 'evil-surround-pairs-alist)
      (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))
    )
  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode))
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

    (add-hook 'emacs-lisp-mode-hook #'cb-evil--init-evil-surround-pairs)

    (evil-define-key 'visual evil-surround-mode-map "s" #'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" #'evil-substitute)))

(use-package evil-iedit-state
  :straight t
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

(use-package evil-ex
  :defer t
  :functions (evil-ex-define-cmd)
  :preface
  (progn
    (defun cb-evil-flyspell-on ()
      "Enable flyspell."
      (interactive)
      (turn-on-flyspell))

    (defun cb-evil-flyspell-off ()
      "Disable flyspell."
      (interactive)
      (turn-off-flyspell)))

  :config
  (progn
    (evil-ex-define-cmd "nospell" #'cb-evil-flyspell-off)
    (evil-ex-define-cmd "spell" #'cb-evil-flyspell-on)))

(use-package evil-ediff
  :straight t
  :after ediff)

(use-package evil-args
  :straight t
  :after evil
  :config
  (progn
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package evil-indent-plus
  :straight t
  :after evil
  :commands (evil-indent-plus-default-bindings)
  :config (evil-indent-plus-default-bindings))

(use-package evil-nerd-commenter
  :straight t
  :commands (evilnc-comment-operator)
  :preface
  (require 'cb-evil-nerd-commenter)
  :init
  (progn
    (evil-global-set-key 'normal (kbd ";") 'evilnc-comment-operator)
    ;; Double all the commenting functions so that the inverse operations
    ;; can be called without setting a flag
    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
    (define-key evil-normal-state-map "gy" 'cb-evil-nerd-commenter/copy-and-comment-lines)

    (spacemacs-keys-set-leader-keys
      "cl" #'cb-evil-nerd-commenter/comment-or-uncomment-lines
      "cL" #'cb-evil-nerd-commenter/comment-or-uncomment-lines-inverse
      "cp" #'cb-evil-nerd-commenter/comment-or-uncomment-paragraphs
      "cP" #'cb-evil-nerd-commenter/comment-or-uncomment-paragraphs-inverse
      "ct" #'cb-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line
      "cT" #'cb-evil-nerd-commenter/quick-comment-or-uncomment-to-the-line-inverse
      "cy" #'cb-evil-nerd-commenter/copy-and-comment-lines
      "cY" #'cb-evil-nerd-commenter/copy-and-comment-lines-inverse)))

(use-package evil-matchit
  :straight t
  :after evil)

(use-package evil-numbers
  :straight t
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt)

  :init
  (progn
    (evil-global-set-key 'normal (kbd "+") #'evil-numbers/inc-at-pt)
    (evil-global-set-key 'normal (kbd "-") #'evil-numbers/dec-at-pt)))

(use-package evil-search-highlight-persist
  :straight t
  :after evil
  :commands (global-evil-search-highlight-persist
             evil-search-highlight-persist-remove-all)

  :preface
  (autoload 'evil-ex-define-cmd "evil-ex")

  :config
  (progn
    (global-evil-search-highlight-persist)
    (evil-ex-define-cmd "noh" #'evil-search-highlight-persist-remove-all)))

(use-package vi-tilde-fringe
  :straight t
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

(use-package cb-evil-shift
  :preface
  (autoload 'evil-visual-state-map "evil-states")
  :bind (:map evil-visual-state-map
              ("<" . cb-evil-shift-left)
              (">" . cb-evil-shift-right)))

(provide 'cb-evil)

;;; cb-evil.el ends here
