;;; config-evil.el --- Configuration for evil-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)

(use-package evil
  :straight t
  :hook (after-init . evil-mode)
  :defer t
  :functions (evil-delete-backward-char-and-join)
  :defines (evil-want-Y-yank-to-eol)
  :preface
  (progn
    (autoload 'evil-visual-update-x-selection "evil-states")
    (general-setq evil-want-keybinding nil)

    (defun config-evil-execute-Q-macro (count)
      "Execute the macro bound to the Q register.

COUNT is the number of repetitions."
      (interactive (list
                    (if current-prefix-arg
                        (if (numberp current-prefix-arg) current-prefix-arg 0)
                      1)))
      (evil-execute-macro count (evil-get-register ?Q t)))

    (defun config-evil-flyspell-on ()
      "Enable flyspell."
      (interactive)
      (turn-on-flyspell))

    (defun config-evil-flyspell-off ()
      "Disable flyspell."
      (interactive)
      (turn-off-flyspell))

    (defun config-evil--bounds-of-surrounding-lines (lines-before lines-after)
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

    (defun config-evil--sp-delete-and-join-compat (fn &rest args)
      (cond
       ;; Narrow before deleting to improve performance in large org buffers.
       ((and (bound-and-true-p smartparens-strict-mode)
             (derived-mode-p 'org-mode))
        (save-restriction
          (apply #'narrow-to-region (config-evil--bounds-of-surrounding-lines 10 10))
          (call-interactively 'sp-backward-delete-char)))

       ((bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char))

       (t
        (apply fn args)))))

  :config
  (progn
    (setq evil-mode-line-format nil)
    (setq-default evil-shift-width 2)
    (setq-default evil-symbol-word-search t)

    (setq evil-want-visual-char-semi-exclusive t)
    (setq evil-want-Y-yank-to-eol t)

    ;; Prevent visual state from updating the clipboard.

    (advice-add #'evil-visual-update-x-selection :override #'ignore)

    ;; Configure cursors.

    (setq evil-motion-state-cursor '("plum3" box))
    (setq evil-visual-state-cursor '("gray" hbar))
    (setq evil-normal-state-cursor '("IndianRed" box))
    (setq evil-insert-state-cursor '("chartreuse3" bar))
    (setq evil-emacs-state-cursor  '("SkyBlue2" (box . t)))

    ;; Motion keys for help buffers.

    (evil-define-key 'motion help-mode-map
      (kbd "<escape>") #'quit-window
      (kbd "^") #'help-go-back
      (kbd "gh") #'help-follow-symbol)

    (evil-global-set-key 'normal (kbd "go") #'browse-url-at-point)

    (evil-global-set-key 'normal (kbd "Q") #'config-evil-execute-Q-macro)

    ;; Initial states and keymaps for builtin Emacs packages.

    (evil-set-initial-state 'tabulated-list-mode 'motion)

    (evil-set-initial-state 'flycheck-error-list-mode 'motion)

    (evil-set-initial-state 'helpful-mode 'motion)

    (evil-set-initial-state 'grep-mode 'normal)

    (evil-set-initial-state 'occur-mode 'normal)
    (with-eval-after-load 'replace
      (evil-add-hjkl-bindings occur-mode-map))

    (evil-set-initial-state 'tar-mode 'emacs)
    (with-eval-after-load 'tar-mode
      (evil-add-hjkl-bindings tar-mode-map))

    (evil-set-initial-state 'profiler-report-mode 'motion)
    (with-eval-after-load 'profiler
      (evil-define-key 'motion profiler-report-mode-map
        "K" 'profiler-report-describe-entry
        "B" 'profiler-report-render-reversed-calltree))

    (with-eval-after-load 'compile
      ;; h (help) binding interferes with evil navigation.
      (evil-define-key 'motion compilation-mode-map (kbd "h") #'evil-backward-char))

    (evil-set-initial-state 'archive-mode 'emacs)
    (with-eval-after-load 'arc-mode
      (evil-define-key 'motion archive-mode-map
        (kbd "q") 'kill-this-buffer
        (kbd "RET") 'archive-extract
        (kbd "o") 'archive-extract-other-window
        (kbd "m") 'archive-mark
        (kbd "x") 'archive-expunge
        (kbd "U") 'archive-unmark-all-files
        (kbd "j") 'archive-next-line
        (kbd "k") 'archive-previous-line))

    (evil-set-initial-state 'wdired-mode 'normal)

    (evil-set-initial-state 'ert-simple-view-mode 'motion)

    (evil-set-initial-state 'diff-mode 'motion)

    (evil-set-initial-state 'haskell-debug-mode 'motion)

    (evil-set-initial-state 'ibuffer-mode 'motion)
    (evil-set-initial-state 'eshell-mode 'insert)

    (evil-set-initial-state 'nix-repl-mode 'insert)

    (evil-set-initial-state 'prodigy-mode 'motion)

    (evil-set-initial-state 'anaconda-mode-view-mode 'motion)

    (evil-set-initial-state 'racer-help-mode 'motion)

    (evil-set-initial-state 'org-agenda-mode 'motion)

    (evil-set-initial-state 'vterm-mode 'emacs)

    ;; Add ex commands for controlling spellcheck.

    (evil-ex-define-cmd "nospell" #'config-evil-flyspell-off)
    (evil-ex-define-cmd "spell" #'config-evil-flyspell-on)

    ;; Better compat with smartparens-strict mode.
    ;; TODO: Move to SP config.

    (advice-add #'evil-delete-backward-char-and-join
                :around #'config-evil--sp-delete-and-join-compat)))

(use-package undo-tree
  :straight t
  :general
  ("C-x t" 'undo-tree-visualize)
  (:states 'normal :keymaps 'org-mode-map
   "C-r" 'undo-tree-redo
   "u" 'undo-tree-undo))

(use-package evil-surround
  :straight t
  :commands (global-evil-surround-mode)
  :defer t
  :after evil
  :general
  (:states 'visual :keymaps 'evil-surround-mode-map
   "s" #'evil-surround-region
   "S" #'evil-substitute)
  :preface
  (defun config-evil--init-evil-surround-pairs ()
    (make-local-variable 'evil-surround-pairs-alist)
    (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))
  :hook
  (emacs-lisp-mode-hook . config-evil--init-evil-surround-pairs)
  :init
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
                  (?t . evil-surround-read-tag)
                  (?< . evil-surround-read-tag)
                  (?f . evil-surround-function)))
  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode +1)))

(use-package evil-bidi
  :defer t
  :after evil)

(use-package evil-collection
  :straight t
  :defer t
  :hook (after-init . evil-collection-init))

(use-package evil-args
  :straight t
  :defer t
  :after evil
  :general (:keymaps
            'evil-inner-text-objects-map "a" #'evil-inner-arg
            :keymaps
            'evil-outer-text-objects-map "a" #'evil-outer-arg))

(use-package evil-matchit
  :straight t
  :defer t
  :after evil
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-numbers
  :straight t
  :after evil
  :defer t
  :general (:states 'normal
            "+" #'evil-numbers/inc-at-pt
            "-" #'evil-numbers/dec-at-pt))

(use-package evil-nerd-commenter
  :straight t
  :defer t
  :after evil
  :general (:states
            'normal
            ";" #'evilnc-comment-operator
            ;; Double all the commenting functions so that the inverse
            ;; operations can be called without setting a flag
            "gc" #'evilnc-comment-operator))

(use-package evil-funcs
  :after evil
  :defer t
  :general (:states 'visual
            "<" #'evil-funcs/shift-left
            ">" #'evil-funcs/shift-right))

(use-package evil-ispell
  :after evil
  :defer t
  :general (:states 'normal
            "zu" #'flyspell-auto-correct-word
            "zU" #'evil-ispell-correct-word
            "zg" #'evil-ispell-mark-word-as-good
            "zG" #'evil-ispell-mark-word-as-locally-good
            "zn" #'evil-ispell-next-spelling-error
            "zp" #'evil-ispell-previous-spelling-error))

(use-package evil-persian
  :after evil
  :general ("<f1>" #'evil-persian-mode))

;; iedit adds useful mass-renaming functionality. This package provides evil
;; compatability.

(use-package evil-iedit-state
  :straight t
  :commands (evil-iedit-state/iedit-mode))


(provide 'config-evil)

;;; config-evil.el ends here
