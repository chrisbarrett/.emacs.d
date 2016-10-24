;;; cb-which-key.el --- Configuration for which-key.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package which-key
  :config
  (progn
    (setq which-key-special-keys nil)
    (setq which-key-use-C-h-commands t)
    (setq which-key-echo-keystrokes 0.02)
    (setq which-key-max-description-length 32)
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (setq which-key-idle-delay 0.4)
    (setq which-key-allow-evil-operators t)

    ;; Rename functions shown by which-key for legibility.

    (defconst cb-which-key--substitutions
      `(
        ;; Trim package prefixes.
        (,(rx "cb/" (group (+ nonl))) . "\\1")
        (,(rx "cb-" (+? nonl) "-" (group (+ nonl))) . "\\1")))

    (dolist (sub cb-which-key--substitutions)
      (push (cons (concat "\\`" (car sub) "\\'") (cdr sub))
            which-key-description-replacement-alist))

    (which-key-add-key-based-replacements
      "SPC ,"   "smartparens"
      "SPC b"   "buffers"
      "SPC c"   "comments"
      "SPC f"   "files"
      "SPC g"   "git/goto"
      "SPC h"   "help"
      "SPC p"   "project"
      "SPC w"   "window"
      "SPC t"   "toggles"
      "SPC SPC" "M-x"
      "SPC m"   '("major-mode-cmd" . "Major mode commands"))

    (which-key-mode +1))

  :functions (which-key-mode which-key-add-key-based-replacements))

(provide 'cb-which-key)

;;; cb-which-key.el ends here
