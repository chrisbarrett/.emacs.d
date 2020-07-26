;;; config-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'subr-x)


;; Define some modes for specific files.

(define-derived-mode dir-locals-mode emacs-lisp-mode "dir-locals")
(add-to-list 'auto-mode-alist '("\\.dir-locals.el\\'" . dir-locals-mode))

(define-derived-mode caskfile-mode emacs-lisp-mode "caskfile")
(add-to-list 'auto-mode-alist '("/Cask\\'" . caskfile-mode))



;; lisp-mode provides definitions of lisp editing modes.

(use-package lisp-mode
  :preface
  (progn
    (defvar calculate-lisp-indent-last-sexp)

    (defun config-elisp--better-lisp-indent-function (indent-point state)
      (let ((normal-indent (current-column))
            (orig-point (point)))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ((and (elt state 2)
               (or (not (looking-at "\\sw\\|\\s_"))
                   (looking-at ":")))
          (unless (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp)
            (goto-char calculate-lisp-indent-last-sexp)
            (beginning-of-line)
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))

          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
         ((and (save-excursion
                 (goto-char indent-point)
                 (skip-syntax-forward " ")
                 (not (looking-at ":")))
               (save-excursion
                 (goto-char orig-point)
                 (looking-at ":")))
          (save-excursion
            (goto-char (+ 2 (elt state 1)))
            (current-column)))
         (t
          (let ((function (buffer-substring (point)
                                            (progn (forward-sexp 1) (point))))
                method)
            (setq method (or (function-get (intern-soft function)
                                           'lisp-indent-function)
                             (get (intern-soft function) 'lisp-indent-hook)))
            (cond ((or (eq method 'defun)
                       (and (null method)
                            (> (length function) 3)
                            (string-match "\\`def" function)))
                   (lisp-indent-defform state indent-point))
                  ((integerp method)
                   (lisp-indent-specform method state
                                         indent-point normal-indent))
                  (method
                   (funcall method indent-point state)))))))))
  :custom
  ((lisp-indent-function #'config-elisp--better-lisp-indent-function)))

;; simple.el is the basic text-editor command package.

(use-package simple
  :defer t
  :mode-hydra
  (emacs-lisp-mode
   ("Eval"
    (("eb" eval-buffer "buffer")
     ("ee" eval-expression "expression")))))

;; debug provides a debugger interface for elisp.

(use-package debug
  :defer t
  :mode-hydra
  (emacs-lisp-mode
   ("Debug"
    (("d" debug-on-entry "on function called")
     ("cd" cancel-debug-on-entry "cancel debug function")
     ("v" debug-on-variable-change "on variable changed")
     ("cv" cancel-debug-on-variable-change "cancel debug variable")))))

;; ERT is the builtin emacs lisp test framework.

(use-package ert
  :defer t
  :mode-hydra
  (emacs-lisp-mode
   ("Test"
    (("tt" (ert t) "run all")
     ("tf" (ert-select-tests :failed t) "run failing")
     ("tn" (ert-select-tests :passed t) "run new")
     ("ts" ert "run with selector")))))


;; IELM is the Elisp repl built in to Emacs.

(use-package ielm
  :general (:keymaps 'emacs-lisp-mode-map "C-c C-z" #'ielm)
  :preface
  (defun config-elisp-pop-to-elisp-buffer ()
    (interactive)
    (if-let* ((buf (seq-find (lambda (buf)
                               (with-current-buffer buf
                                 (derived-mode-p 'emacs-lisp-mode)))
                             (buffer-list))))
        (pop-to-buffer buf)
      (user-error "No Emacs Lisp buffers")))
  :config
  (progn
    (add-hook 'inferior-emacs-lisp-mode-hook #'hs-minor-mode)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*ielm*" eos)
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (side            . right)
                   (window-width    . 80)))

    (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-z") #'config-elisp-pop-to-elisp-buffer)))

;; elisp-slime-nav provides a command for navigating to the definitions of
;; things in elisp in a uniform way.

(use-package elisp-slime-nav
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  :general
  (:keymaps 'emacs-lisp-mode-map :states 'normal
   "M-." #'elisp-slime-nav-find-elisp-thing-at-point))

;; eldoc shows function parameters in the minibuffer.

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

;; nameless hides the package prefix in elisp buffers, which can make things
;; easier to read.

(use-package nameless
  :defines (nameless-current-name)
  :commands nameless-mode
  :hook (emacs-lisp-mode . nameless-mode)
  :config
  (progn
    (add-to-list 'display-buffer-alist
                 `(,(rx "*Ilist*")
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width   . 0.25)))

    (setq nameless-prefix ":")
    (setq nameless-private-prefix t)))

;; debug.el defines the emacs lisp debugger.

(use-package debug
  :defer t
  :general (:keymaps 'debugger-mode-map
            "," 'major-mode-hydra--debugger-mode/body
            "j" 'forward-to-indentation
            "k" 'backward-to-indentation)
  :init
  (add-hook 'debugger-mode-hook #'hl-line-mode)
  :config
  (progn
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Backtrace*" eos)
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (side            . right)
                   (window-width    . 80)))

    (major-mode-hydra-define debugger-mode nil
      ("Control"
       (("," debugger-step-through "step")
        ("b" debugger-frame "enter frame")
        ("c" debugger-continue "continue")
        ("j" debugger-jump "jump")
        ("u" debugger-frame-clear "stop on frame"))

       "Values"
       (("e" debugger-eval-expression "eval...")
        ("R" debugger-record-expression "eval and record...")
        ("r" debugger-return-value "return..."))

       "Assistance"
       (("l" debugger-list-functions "list functions")
        ("v" debugger-toggle-locals "toggle vars"))))))

;; helpful is a more feature-rich alternative to the Emacs Lisp help buffer.

(use-package helpful
  :commands (helpful-command helpful-key helpful-variable helpful-callable)
  :general
  (:keymaps '(emacs-lisp-mode-map helpful-mode-map) :states '(motion normal)
   "K" 'helpful-at-point)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helpful ")
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (slot            . 1)
                 (window-width    . 0.5))))

;; rainbow mode shows the colour for hex strings.

(use-package rainbow-mode
  :hook (emacs-lisp-mode . rainbow-mode)
  :preface
  (defun config-elisp--on-rainbow-mode (&rest arg)
    (when (and arg (bound-and-true-p bug-reference-prog-mode))
      (bug-reference-prog-mode -1)))
  :config
  (advice-add 'rainbow-mode :after #'config-elisp--on-rainbow-mode))

;; pp provides pretty-printing of lisp forms

(use-package pp
  :defer t
  :general
  (:keymaps '(emacs-lisp-mode-map ielm-map) :states '(motion normal insert)
   "C-<return>" 'pp-eval-last-sexp
   "C-c <return>" 'pp-macroexpand-last-sexp))

(use-package buttercup
  :after elisp-mode
  :preface
  (defun config-elisp--ad-buttercup-handle-ansi-codes (&rest _)
    (with-current-buffer "*Buttercup*"
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  :config
  (advice-add #'buttercup-reporter-batch--print-summary :after
              #'config-elisp--ad-buttercup-handle-ansi-codes))


(provide 'config-elisp)

;;; config-elisp.el ends here
