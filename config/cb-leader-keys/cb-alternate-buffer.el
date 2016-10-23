;;; cb-alternate-buffer.el --- Command for toggling between buffers.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'seq)

(defun cb/alternate-buffer (&optional window)
  "Toggle back and forth between two buffers.

WINDOW sets the window in which to toggle, and defaults to the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (car (seq-filter (lambda (buffer)
                            (and (not (eq buffer current-buffer))
                                 (or (null buffer-predicate) (funcall buffer-predicate buffer))))
                          (seq-map #'car (window-prev-buffers window))))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))


(provide 'cb-alternate-buffer)

;;; cb-alternate-buffer.el ends here
