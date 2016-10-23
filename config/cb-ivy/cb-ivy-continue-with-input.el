;;; cb-ivy-continue-with-input.el --- Force ivy to continue with user input.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'ivy)

(defun cb-ivy-continue-with-input ()
  "Use the current input instead of the candidate."
  (interactive)
  (delete-minibuffer-contents)
  (insert (setq ivy--current
                (if ivy--directory
                    (expand-file-name ivy-text ivy--directory)
                  ivy-text))))

(provide 'cb-ivy-continue-with-input)

;;; cb-ivy-continue-with-input.el ends here
