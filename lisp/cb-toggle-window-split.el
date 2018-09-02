;;; cb-toggle-window-split.el --- Command to toggle window split.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defun cb/toggle-window-split ()
  "Toggle between vertical and horizontal split."
  (interactive)
  (cond
   ((one-window-p)
    (user-error "Only one window"))
   ((> (count-windows) 2)
    (user-error "Too many windows to toggle split"))
   (t
    (let* ((b1 (window-buffer))
           (b2 (window-buffer (next-window)))
           (w1-edges (window-edges (selected-window)))
           (w2-edges (window-edges (next-window)))
           (w2 (not (and (<= (car w1-edges) (car w2-edges))
                         (<= (cadr w1-edges) (cadr w2-edges)))))
           (split-fn
            (if (= (car w1-edges)
                   (car (window-edges (next-window))))
                #'split-window-horizontally
              #'split-window-vertically)))

      (delete-other-windows)
      (let ((w1 (selected-window)))
        (funcall split-fn)
        (when w2 (other-window 1))
        (set-window-buffer (selected-window) b1)
        (set-window-buffer (next-window) b2)
        (select-window w1)
        (when w2 (other-window 1)))))))

(provide 'cb-toggle-window-split)

;;; cb-toggle-window-split.el ends here
