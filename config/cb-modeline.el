;;; cb-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:


;; Hide the mode line by default.

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line."
  :global t
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  (redraw-display))

(define-globalized-minor-mode global-hidden-mode-line-mode hidden-mode-line-mode
  hidden-mode-line-mode)

(global-hidden-mode-line-mode +1)


(provide 'cb-modeline)

;;; cb-modeline.el ends here
