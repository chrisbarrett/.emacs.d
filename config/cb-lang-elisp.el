;;; cb-lang-elisp.el --- Configuration for emacs-lisp  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)

(use-package elisp-mode
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   "C-c C-c" #'eval-defun
   "C-c C-b" #'eval-buffer)
  (:states 'visual
   :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   "C-c C-c" #'eval-region)
  :custom
  (elisp-flymake-byte-compile-load-path (cons "./" load-path)))

(mode-leader-set-key :keymaps 'emacs-lisp-mode-map
  "e" '(nil :wk "eval")
  "eb" '(eval-buffer :wk "eval buf")

  "d" '(nil :wk "debug")

  ;; breakpoints
  "db" '(nil :wk "breakpoints")
  "dbb" '(edebug-set-breakpoint :wk "set")
  "dbu" '(edebug-unset-breakpoint :wk "unset")
  "dbU" '(edebug-unset-breakpoints :wk "unset (all)")
  "dbn" '(edebug-next-breakpoint :wk "next")
  "dbc" '(edebug-set-conditional-breakpoint :wk "conditional")
  "dbC" '(edebug-set-global-break-condition :wk "conditional (global)")
  "dbt" '(edebug-toggle-disable-breakpoint :wk "toggle")

  "dc" '(edebug-continue-mode :wk "continue")
  "dC" '(edebug-Continue-fast-mode :wk "continue (fast)")
  "dd" '(edebug-pop-to-backtrace :wk "backtrace")

  "df" '(edebug-defun :wk "instrument function (at pt)")
  "dF" '(edebug-on-entry :wk "instrument function...")
  "de" '(edebug-eval-top-level-form :wk "eval & step through")

  "dm" '(nil :wk "mode")
  "dmc" '(edebug-continue-mode :wk "continue")
  "dmC" '(edebug-Continue-fast-mode :wk "continue (fast)")
  "dms" '(edebug-step-mode :wk "step")
  "dmn" '(edebug-next-mode :wk "next")
  "dmg" '(edebug-go-mode :wk "go")
  "dmG" '(edebug-Go-nonstop-mode :wk "go (non-stop)")
  "dmt" '(edebug-trace-mode :wk "trace")
  "dmT" '(edebug-Trace-fast-mode :wk "trace (fast)")

  "dg" '(edebug-where :wk "where")
  "dn" '(edebug-goto-here :wk "forward to stop point")
  "dt" '(edebug-toggle-disable-breakpoint :wk "toggle")
  "dI" '(edebug-instrument-callee :wk "instrument callee")
  "di" '(edebug-step-in :wk "step in")
  "do" '(edebug-step-out :wk "step out")

  ;; quitting and stopping
  "dq" '(edebug-top-level-nonstop :wk "toplevel")
  "ds" '(edebug-stop :wk "stop")

  ;; evaluation
  "dr" '(edebug-previous-result :wk "previous result")
  "de" '(edebug-eval-expression :wk "eval... (expression)")
  "dl" '(edebug-visit-eval-list :wk "switch to eval list")

  "d?" '(edebug-help :wk "help")

  "t" 'ert)

(use-package ielm
  :hook (ielm-mode . hs-minor-mode)
  :general
  (:keymaps 'ielm-map
   "C-c C-k" 'quit-window)

  ;; Pop back and forth between IELM and Elisp buffers with ~C-c C-z~
  :preface
  (defun cb-pop-to-elisp-buffer ()
    (interactive)
    (if-let* ((buf (seq-find (lambda (buf)
                               (with-current-buffer buf
                                 (derived-mode-p 'emacs-lisp-mode)))
                             (buffer-list))))
        (pop-to-buffer buf)
      (user-error "No Emacs Lisp buffers")))

  :general
  (:keymaps 'emacs-lisp-mode-map "C-c C-z" #'ielm)
  (:keymaps 'ielm-map
   "C-c C-z" #'cb-pop-to-elisp-buffer)

  ;; Teach xref to work correctly in IELM buffers
  :hook (ielm-mode . config-ielm-xref-setup)
  :preface
  (defun config-ielm-xref-setup ()
    (setq-local xref-backend-functions '(elisp--xref-backend t))))

(use-package pp
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map ielm-mode-map)
   :states '(motion normal insert)
   "C-c C-<return>" 'pp-eval-last-sexp
   "C-c <return>" 'pp-eval-last-sexp
   "C-c C-e" 'pp-macroexpand-last-sexp))



;;; Debuggers

(mode-leader-set-key :keymaps 'debugger-mode-map
  "," '(debugger-step-through :wk "step")
  "b" '(debugger-frame :wk "frame")
  "c" '(debugger-continue :wk "continue")
  "j" '(debugger-jump :wk "jump")
  "u" '(debugger-frame-clear :wk "clear frame")

  "e" '(debugger-eval-expression :wk "eval...")
  "R" '(debugger-record-expression :wk "record...")
  "r" '(debugger-return-value :wk "return...")

  "l" '(debugger-list-functions :wk "list functions")
  "v" '(debugger-toggle-locals :wk "toggle locals"))

(use-package edebug
  ;; Temporarily hide git-gutter while debugging.
  :preface
  (defvar-local edebug-git-gutter-enabled-at-start-p nil)
  (autoload 'git-gutter-mode "git-gutter")
  (define-advice edebug-mode (:around (fn &rest args) toggle-git-gutter)
    (unwind-protect (apply fn args)
      (pcase `(,edebug-mode ,edebug-git-gutter-enabled-at-start-p)
        (`(t ,_) ; edebug started - disable git-gutter
         (setq-local edebug-git-gutter-enabled-at-start-p t)
         (git-gutter-mode -1))
        (`(nil t) ; edebug finished - enable git-gutter
         (git-gutter-mode +1))))))



(use-package lisp-mode
  :preface
  (progn
    (defvar calculate-lisp-indent-last-sexp)

    (defun better-elisp-indent-function (indent-point state)
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
  (lisp-indent-function #'better-elisp-indent-function))

(use-package checkdoc
  :custom
  (checkdoc-force-docstrings-flag nil)
  (checkdoc-arguments-in-order-flag nil))

;; This always fails since I don't use =package.el=.

(advice-add 'package-lint--check-packages-installable :override #'ignore)

(provide 'cb-lang-elisp)

;;; cb-lang-elisp.el ends here
