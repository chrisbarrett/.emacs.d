;;; header-line-mode.el --- Minor mode for toggling the header line.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst header-line-function #'ignore
  "0-argument function returning the header line format string.")

;;;###autoload
(define-minor-mode header-line-mode
  "Minor mode to show or hide the header line."
  nil nil nil
  (if header-line-mode
      (setq header-line-format (funcall header-line-function))
    (setq header-line-format nil)))

;;;###autoload
(defun header-line-mode-on ()
  "Explicitly enable `header-line-mode'."
  (interactive)
  (header-line-mode +1))

;;;###autoload
(define-globalized-minor-mode header-line-global-mode header-line-mode
  header-line-mode-on)

(provide 'header-line-mode)

;;; header-line-mode.el ends here
