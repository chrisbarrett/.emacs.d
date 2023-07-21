;;; cb-evil.el --- Configuration for evil (modal editing) -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; `evil' provides macros that I want to use in `:config' blocks, so teach the
;; byte-compiler about them to avoid warnings.

(require 'autoloads)
(require 'cl-lib)

(use-package evil :ensure t :hook (after-init . evil-mode)

  ;; Initial states
  :config
  (evil-set-initial-state 'ert-simple-view-mode 'motion)
  (evil-set-initial-state 'haskell-debug-mode 'motion)
  (evil-set-initial-state 'nix-repl-mode 'insert)
  (evil-set-initial-state 'occur-mode 'normal)
  (evil-set-initial-state 'tabulated-list-mode 'motion)
  (with-eval-after-load 'replace
    (evil-add-hjkl-bindings occur-mode-map))

  :custom
  (evil-mode-line-format nil)
  (evil-shift-width 2)
  (evil-undo-system 'undo-redo)
  (evil-symbol-word-search t)
  (evil-want-visual-char-semi-exclusive t)
  (evil-want-Y-yank-to-eol t)
  (evil-visual-update-x-selection-p t)
  (evil-motion-state-cursor '("plum3" box))
  (evil-visual-state-cursor '("gray" hbar))
  (evil-normal-state-cursor '("IndianRed" box))
  (evil-insert-state-cursor '("chartreuse3" bar))
  (evil-emacs-state-cursor  '("SkyBlue2" (box . t)))

  ;; Suppress evil's own keybinding configuration; evil-collection is used
  ;; instead.
  (evil-want-keybinding nil)
  (evil-want-integration t)

  ;; Execute macro bound to `q' with `Q'

  ;; Use `Q' in normal state to execute the macro bound to `q' register. This is
  ;; a convenient way to quickly define a macro, then execute it
  ;; immediately--just double-tap `q' to record, then hit `Q' to execute.
  :preface
  (defun cb-evil-execute-Q-macro (count)
    "Execute the macro bound to the Q register COUNT times."
    (interactive (list
                  (if current-prefix-arg
                      (if (numberp current-prefix-arg) current-prefix-arg 0)
                    1)))
    (evil-execute-macro count (evil-get-register ?Q t)))

  :general

  (:states 'normal
   "C-w /" #'evil-window-vnew
   "go" #'browse-url-at-point
   "Q" #'cb-evil-execute-Q-macro)

  (:states '(normal insert visual motion)
   "M-e" 'evil-window-next)

  :preface
  (defun cb-evil-beginning-of-defun ()
    (interactive)
    (beginning-of-defun)
    (back-to-indentation))
  :general
  (:states 'normal :keymaps 'prog-mode-map
   "(" 'cb-evil-beginning-of-defun
   ")" 'end-of-defun)

  ;; Add vim-style `:spell' and `:nospell' ex commands
  :preface
  (defun cb-flyspell-on ()
    "Enable flyspell."
    (interactive)
    (turn-on-flyspell))

  (defun cb-flyspell-off ()
    "Disable flyspell."
    (interactive)
    (turn-off-flyspell))

  :config
  (evil-ex-define-cmd "nospell" #'cb-flyspell-off)
  (evil-ex-define-cmd "spell" #'cb-flyspell-on)

  ;; Teach < and > to shift text in a context-sensitive way

  :preface
  (defun cb-evil-shift-left (&optional beg end)
    "Shift left, keeping the region active.
  BEG and END are the bounds of the active region."
    (interactive "r")
    (evil-shift-left beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun cb-evil-shift-right (&optional beg end)
    "Shift right, keeping the region active.
  BEG and END are the bounds of the active region."
    (interactive "r")
    (evil-shift-right beg end)
    (evil-normal-state)
    (evil-visual-restore))

  :general (:states 'visual
            "<" #'cb-evil-shift-left
            ">" #'cb-evil-shift-right)

  ;; Replace rectangle-mark-mode binding with insert-char
  ;;
  ;; Since I have visual state for block selection, I don't need a rectangle
  ;; selection key. But a nice way to insert Unicode chars would be good.
  :general ("C-x SPC" #'insert-char))

(use-package evil-collection :ensure t :demand t :after evil
  :config
  (delete 'elisp-slime-nav evil-collection-mode-list)
  (evil-collection-init))

(use-package evil-surround :ensure t :demand t :after evil
  :config
  (global-evil-surround-mode +1)
  :preface
  (defun cb-evil-surround-backtick ()
    (cons "`"
          (if (derived-mode-p 'emacs-lisp-mode)
              "'"
            "`")))
  :custom
  (evil-surround-pairs-alist '((?` . cb-evil-surround-backtick)
                               (?\( . ("(" . ")"))
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

  :general
  (:states 'visual :keymaps 'evil-surround-mode-map
   "s" #'evil-surround-region
   "S" #'evil-substitute)

  ;; Prevent evil-surround from interfering with magit hunk operations.
  :config
  (define-advice evil-surround-mode (:filter-args (&rest args) magit-compat)
    (if (derived-mode-p 'magit-section-mode)
        (list -1)
      args)))

(use-package evil-matchit :ensure t :demand t :after evil
  :commands (global-evil-matchit-mode)
  :config
  (global-evil-matchit-mode +1))

(use-package evil-ispell
  :general (:states 'normal
            "z SPC" #'flyspell-auto-correct-word
            "zU" #'evil-ispell-correct-word
            "zg" #'evil-ispell-mark-word-as-good
            "zG" #'evil-ispell-mark-word-as-locally-good
            "zn" #'evil-ispell-next-spelling-error
            "zp" #'evil-ispell-previous-spelling-error))

(use-package evil-numbers :ensure t :demand t :after evil
  :general (:states 'normal
            "+" #'evil-numbers/inc-at-pt
            "-" #'evil-numbers/dec-at-pt))

(use-package evil-iedit-state :ensure t
  :commands (evil-iedit-state/iedit-mode)
  :general
  (:keymaps 'evil-iedit-state-map
   "P" 'evil-iedit-state/paste-replace
   "p" nil))

(use-package evil-nerd-commenter :ensure t)

(use-package evil-goggles :ensure t :demand t :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(provide 'cb-evil)

;;; cb-evil.el ends here
