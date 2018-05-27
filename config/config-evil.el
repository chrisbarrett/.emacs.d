;;; config-evil.el --- Configuration for evil-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'config-hydras)
(require 'general)

(straight-use-package 'link-hint)

(declare-function evil-delay "evil-common")
(declare-function evil-set-initial-state "evil-core")



(require 'evil-hacks)

(use-package evil
  :straight t
  :functions (evil-mode evil-delay evil-delete-backward-char-and-join)
  :defines (evil-want-Y-yank-to-eol)
  :defer 1
  :preface
  (progn
    (autoload 'evil-visual-update-x-selection "evil-states")

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

    ;; Initial states and keymaps for builtin Emacs packages.

    (evil-set-initial-state 'flycheck-error-list-mode 'motion)

    (evil-set-initial-state 'grep-mode 'normal)

    (evil-set-initial-state 'occur-mode 'normal)
    (with-eval-after-load 'replace
      (evil-add-hjkl-bindings occur-mode-map))

    (evil-set-initial-state 'tar-mode 'emacs)
    (with-eval-after-load 'tar-mode
      (evil-add-hjkl-bindings tar-mode-map))

    (evil-set-initial-state 'archive-mode 'emacs)
    (with-eval-after-load 'arc-mode
      (evil-add-hjkl-bindings archive-mode-map))

    (evil-set-initial-state 'profiler-report-mode 'motion)
    (with-eval-after-load 'profiler
      (evil-define-key 'motion profiler-report-mode-map
        "j" 'profiler-report-next-entry
        "k" 'profiler-report-previous-entry
        "n" 'profiler-report-next-entry
        "p" 'profiler-report-previous-entry

        (kbd "TAB") 'profiler-report-toggle-entry
        (kbd "K") 'profiler-report-describe-entry
        (kbd "RET") 'profiler-report-find-entry
        (kbd "=") 'profiler-report-compare-profile

        "g r" 'revert-buffer
        "B" 'profiler-report-render-reversed-calltree
        "f" 'profiler-report-find-entry))

    (with-eval-after-load 'compile
      ;; h (help) binding interferes with evil navigation.
      (evil-define-key 'motion compilation-mode-map (kbd "h") #'evil-backward-char))

    (evil-set-initial-state 'doc-view-mode 'motion)
    (with-eval-after-load 'docview
      (evil-define-key 'motion doc-view-mode-map
        (kbd "gg") 'doc-view-first-page
        (kbd "G") 'doc-view-last-page
        (kbd "j") 'doc-view-next-line-or-next-page
        (kbd "k") 'doc-view-previous-line-or-previous-page
        (kbd "h") 'image-backward-hscroll
        (kbd "l") 'image-forward-hscroll
        (kbd "n") 'doc-view-next-page
        (kbd "p") 'doc-view-previous-page
        (kbd "<down>") 'doc-view-next-line-or-next-page
        (kbd "<up>") 'doc-view-previous-line-or-previous-page
        (kbd "<left>") 'image-backward-hscroll
        (kbd "<right>") 'image-forward-hscroll))

    (with-eval-after-load 'archive-mode
      ;; KLUDGE: `evil-set-initial-state' doesn't work with archive-mode. Set in a hook instead.
      (add-hook 'archive-mode-hook 'evil-motion-state)
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

    (evil-set-initial-state 'mu4e-main-mode 'emacs)
    (evil-set-initial-state 'mu4e-headers-mode 'emacs)
    (evil-set-initial-state 'mu4e-view-mode 'motion)

    (evil-set-initial-state 'nix-repl-mode 'insert)

    (evil-set-initial-state 'prodigy-mode 'motion)

    (evil-set-initial-state 'anaconda-mode-view-mode 'motion)

    (evil-set-initial-state 'racer-help-mode 'motion)

    (evil-set-initial-state 'indium-inspector-mode 'motion)
    (evil-set-initial-state 'indium-repl-mode 'insert)

    (evil-set-initial-state 'org-agenda-mode 'motion)

    ;; Add ex commands for controlling spellcheck.

    (evil-ex-define-cmd "nospell" #'config-evil-flyspell-off)
    (evil-ex-define-cmd "spell" #'config-evil-flyspell-on)

    ;; Better compat with smartparens-strict mode.
    ;; TODO: Move to SP config.

    (advice-add #'evil-delete-backward-char-and-join
                :around #'config-evil--sp-delete-and-join-compat)))

(use-package evil-surround
  :straight t
  :after evil-common
  :commands (global-evil-surround-mode)
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
  :config
  (global-evil-surround-mode +1))

(use-package evil-iedit-state
  :straight t
  :commands (evil-iedit-state/iedit-mode)
  :after evil-common
  :config
  (progn
    (general-setq iedit-current-symbol-default t
                  iedit-only-at-symbol-boundaries t
                  iedit-toggle-key-default nil)

    ;; Enable leader key in iedit and iedit-insert states
    (config-hydras-insinuate evil-iedit-state-map)))

(use-package evil-ediff
  :straight t
  :after (:and ediff evil-common))

(use-package evil-args
  :straight t
  :after evil-common
  :general (:keymaps
            'evil-inner-text-objects-map "a" #'evil-inner-arg
            :keymaps
            'evil-outer-text-objects-map "a" #'evil-outer-arg))

(use-package evil-matchit
  :straight t
  :after evil-common)

(use-package evil-numbers
  :straight t
  :after evil-common
  :general (:states 'normal
            "+" #'evil-numbers/inc-at-pt
            "-" #'evil-numbers/dec-at-pt))

(use-package evil-search-highlight-persist
  :straight t
  :after evil-common
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
  :after evil-common
  :commands (vi-tilde-fringe-mode global-vi-tilde-fringe-mode)
  :preface
  (defun config-evil--vi-tilde-fringe-off-if-readonly (args)
    (if buffer-read-only
        '(-1)
      args))
  :config
  (progn
    (advice-add 'vi-tilde-fringe-mode :filter-args #'config-evil--vi-tilde-fringe-off-if-readonly)
    (global-vi-tilde-fringe-mode)))

(use-package evil-nerd-commenter
  :straight t
  :after evil-common
  :general (:states
            'normal
            ";" #'evilnc-comment-operator
            ;; Double all the commenting functions so that the inverse
            ;; operations can be called without setting a flag
            "gc" #'evilnc-comment-operator))

(use-package evil-funcs
  :after evil-common
  :general (:states 'visual
            "<" #'evil-funcs/shift-left
            ">" #'evil-funcs/shift-right))

(provide 'config-evil)

;;; config-evil.el ends here
