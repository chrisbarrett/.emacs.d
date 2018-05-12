;;; hidden-mode-line.el --- Minor mode to hide the mode line.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

;;;###autoload
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

;;;###autoload
(define-globalized-minor-mode global-hidden-mode-line-mode hidden-mode-line-mode
  hidden-mode-line-mode)

(provide 'hidden-mode-line)

;;; hidden-mode-line.el ends here
