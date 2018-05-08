;;; cb-gud.el --- Configuration for GUD  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'evil-transient-state)
(require 'spacemacs-keys)
(require 'subr-x)

(use-package realgud
  :straight t
  :commands (realgud:cmd-newer-frame
             realgud:cmd-older-frame
             realgud:cmd-backtrace
             realgud:cmd-enable
             realgud:cmd-disable
             realgud:cmd-shell
             realgud:cmd-step
             realgud:cmd-until
             realgud:cmd-delete
             realgud:cmd-break
             realgud:cmd-continue
             realgud:cmd-quit
             realgud:cmd-restart
             realgud:cmd-eval-dwim
             realgud:cmd-finish
             realgud-recenter-arrow
             realgud:cmd-next
             realgud:cmd-step
             realgud:cmd-repeat-last)
  :functions
  (realgud:run-process
   realgud-get-cmdbuf
   realgud-get-current-srcbuf
   realgud:cmd-terminate
   realgud-get-srcbuf)

  :init
  (spacemacs-keys-set-leader-keys "D" #'debugger-transient-state/body)
  :preface
  (progn
    (autoload 'f-relative "f")

    (defun cb-gud--realgud-command-for-mode (mode)
      (pcase mode
        (`python-mode
         (lambda ()
           (let* ((file (shell-quote-argument (f-relative (buffer-file-name))))
                  (args (list "python" "-m" "pdb" file)))
             (realgud:run-process "pdb" (buffer-file-name) args 'realgud:pdb-minibuffer-history))))))

    (defun realgud ()
      (interactive)
      (let ((buf (current-buffer)))
        (unless (or (realgud-get-cmdbuf) (realgud-get-current-srcbuf))
          (if-let (command (cb-gud--realgud-command-for-mode major-mode))
              (funcall command)
            (error "No realgud support for %s" major-mode)))
        (cb-gud--setup-realgud-windows buf)))

    (defun cb-gud-realgud-terminate ()
      (interactive)
      (let ((cmd-buffer (realgud-get-cmdbuf)))
        (realgud:cmd-terminate)
        (when (buffer-live-p cmd-buffer)
          (with-current-buffer cmd-buffer
            (when-let* ((windows (get-buffer-window-list cmd-buffer)))
              (ignore-errors
                (dolist (w windows)
                  (delete-window w))))
            (let ((kill-buffer-query-functions nil))
              (kill-buffer cmd-buffer))))))

    (defun cb-gud--setup-realgud-windows (&optional buffer)
      (interactive)
      (let* ((buffer (or buffer (current-buffer)))
             (src-buffer (realgud-get-srcbuf buffer))
             (cmd-buffer (realgud-get-cmdbuf buffer)))
        (display-buffer cmd-buffer)
        (select-window (display-buffer src-buffer)))))

  :config
  (progn
    (setq realgud-safe-mode nil)
    (setq realgud-populate-common-fn-keys-function nil)
    (defalias 'realgud-window-src-undisturb-cmd #'cb-gud--setup-realgud-windows)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*" "pdb " (+? nonl) "*" eos)
                   (display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width    . 0.5))))

  :preface
  (evil-transient-state-define debugger
    :title "Debugger"
    :on-enter (realgud)
    :doc "
[_q_] quit    [_X_] terminate  [_R_] restart
[_._] repeat  [_l_] recenter   [_!_] shell

Frames    ^^^^              Stepping/Running ^^^^        Breakpoints ^^^^
──────────^^^^───────────── ─────────────────^^^^─────── ────────────^^^^─────────────
[_<_] up  [_>_] down        [_s_] step  [_n_] over       [_RET_] insert  [_d_] delete
[_c_] callstack        ^^   [_f_] finish frame ^^        [_+_] enable    [_-_] disable
[_e_] eval             ^^   [_r_] resume
                       ^^^^ [_L_] run to this line

"
    :foreign-keys run
    :bindings
    ("<" realgud:cmd-newer-frame)
    (">" realgud:cmd-older-frame)
    ("c" realgud:cmd-backtrace)
    ("+" realgud:cmd-enable)
    ("-" realgud:cmd-disable)
    ("!" realgud:cmd-shell)
    ("s" realgud:cmd-step)
    ("L" realgud:cmd-until)
    ("d" realgud:cmd-delete)
    ("RET" realgud:cmd-break)
    ("r" realgud:cmd-continue)
    ("q" cb-gud-realgud-terminate :exit t)
    ("X" realgud:cmd-quit :exit t)
    ("R" realgud:cmd-restart)
    ("e" realgud:cmd-eval-dwim)
    ("f" realgud:cmd-finish)
    ("l" realgud-recenter-arrow)
    ("n" realgud:cmd-next)
    ("s" realgud:cmd-step)
    ("." realgud:cmd-repeat-last)))

(provide 'cb-gud)

;;; cb-gud.el ends here
