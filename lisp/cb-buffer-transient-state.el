;;; cb-buffer-transient-state.el --- Buffer navigation transient state.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'evil-transient-state)

(evil-transient-state-define cb-buffer
  :title "Buffer Selection Transient State"
  :bindings
  ("n" next-buffer "next")
  ("N" previous-buffer "previous")
  ("p" previous-buffer "previous")
  ("k" kill-this-buffer "kill")
  ("q" nil "quit" :exit t))

(provide 'cb-buffer-transient-state)

;;; cb-buffer-transient-state.el ends here
