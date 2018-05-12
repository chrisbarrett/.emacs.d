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


;;; Commenting support

(defun evil-funcs/comment-or-uncomment-lines-inverse (&optional arg)
  "Comment or uncomment lines.
ARG is passed to `evilnc-comment-or-uncomment-lines'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-lines arg)))

(defun evil-funcs/comment-or-uncomment-lines (&optional arg)
  "Comment or uncomment lines.
ARG is passed to `evilnc-comment-or-uncomment-lines'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-lines arg)))

(defun evil-funcs/copy-and-comment-lines-inverse (&optional arg)
  "Copy and comment lines.
ARG is passed to `evilnc-copy-and-comment-lines'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-copy-and-comment-lines arg)))

(defun evil-funcs/copy-and-comment-lines (&optional arg)
  "Copy and comment lines.
ARG is passed to `evilnc-copy-and-comment-lines'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-copy-and-comment-lines arg)))

(defun evil-funcs/quick-comment-or-uncomment-to-the-line-inverse (&optional arg)
  "Comment or uncomment lines.
ARG is passed to `evilnc-comment-or-uncomment-to-the-line'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-to-the-line arg)))

(defun evil-funcs/quick-comment-or-uncomment-to-the-line (&optional arg)
  "Comment or uncomment lines.
ARG is passed to `evilnc-comment-or-uncomment-to-the-line'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-to-the-line arg)))

(defun evil-funcs/comment-or-uncomment-paragraphs-inverse (&optional arg)
  "Comment or uncomment paragraphs.
ARG is passed to `evilnc-comment-or-uncomment-paragraphs-inverse'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-paragraphs arg)))

(defun evil-funcs/comment-or-uncomment-paragraphs (&optional arg)
  "Comment or uncomment paragraphs.
ARG is passed to `evilnc-comment-or-uncomment-paragraphs'."
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-paragraphs arg)))

(provide 'evil-funcs)

;;; evil-funcs.el ends here
