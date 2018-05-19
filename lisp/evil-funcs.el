;;; evil-funcs.el --- Supporting functions for evil-nerd-commenter.

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'evil)
(require 'evil-nerd-commenter)


;;; Shifting support

(defun evil-funcs/shift-left (&optional beg end)
  "Shift left, keeping the region active.

BEG and END are the bounds of the active region."
  (interactive "r")
  (evil-shift-left beg end)
  (evil-normal-state)
  (evil-visual-restore))

(defun evil-funcs/shift-right (&optional beg end)
  "Shift right, keeping the region active.

BEG and END are the bounds of the active region."
  (interactive "r")
  (evil-shift-right beg end)
  (evil-normal-state)
  (evil-visual-restore))

(provide 'evil-funcs)

;;; evil-funcs.el ends here
