;;; cb-header-line-mode.el --- Minor mode for toggling the header line.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst cb-header-line-function #'ignore
  "0-argument function returning the header line format string.")

;;;###autoload
(define-minor-mode cb-header-line-mode
  "Minor mode to show or hide the header line."
  nil nil nil
  (if cb-header-line-mode
      (setq header-line-format (funcall cb-header-line-function))
    (setq header-line-format nil)))

;;;###autoload
(defun cb-header-line-mode-on ()
  "Explicitly enable `cb-header-line-mode'."
  (interactive)
  (cb-header-line-mode +1))

;;;###autoload
(define-globalized-minor-mode cb-header-line-global-mode cb-header-line-mode
  cb-header-line-mode-on)

(provide 'cb-header-line-mode)

;;; cb-header-line-mode.el ends here
