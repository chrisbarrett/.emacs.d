;;; mode-line-mode.el --- Minor mode for toggling the mode line.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst mode-line-function #'ignore
  "0-argument function returning the mode line format string.")

;;;###autoload
(define-minor-mode mode-line-mode
  "Minor mode to show or hide the mode line."
  nil nil nil
  (if mode-line-mode
      (setq mode-line-format (funcall mode-line-function))
    (setq mode-line-format nil)))

;;;###autoload
(defun mode-line-mode-on ()
  "Explicitly enable `mode-line-mode'."
  (interactive)
  (mode-line-mode +1))

;;;###autoload
(defun mode-line-mode-off ()
  "Explicitly disable `mode-line-mode'."
  (interactive)
  (mode-line-mode -1))

;;;###autoload
(define-globalized-minor-mode mode-line-global-mode mode-line-mode
  mode-line-mode-on)

(provide 'mode-line-mode)

;;; mode-line-mode.el ends here
