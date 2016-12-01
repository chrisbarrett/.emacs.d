;;; cb-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package hidden-mode-line
  :commands (hidden-mode-line-mode global-hidden-mode-line-mode))

(setq-default mode-line-format " ")

;; Command to toggle the display of the mode-line as a header

(defconst cb-header-line-format " %3l %* %[%b%] %n")

(defun cb-toggle-header-line ()
  "Toggle the header line on or off."
  (interactive)
  (if header-line-format
      (setq header-line-format nil)
    (setq header-line-format cb-header-line-format))
  (set-window-buffer nil (current-buffer)))

(spacemacs-keys-set-leader-keys "tm" #'cb-toggle-header-line)

(provide 'cb-modeline)

;;; cb-modeline.el ends here
