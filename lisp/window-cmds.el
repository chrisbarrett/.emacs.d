;;; window-cmds.el --- Window management commands.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Window management commands from Steve Purcell's config.
;;
;; https://github.com/purcell/emacs.d/blob/80011b6/lisp/init-windows.el

;;; Code:

(defun window-cmds-split-horizontally (&optional arg)
  "When splitting window, show the other buffer in the new window.

With prefix arg ARG, don't select the new window."
  (interactive "P")
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer))
    (unless arg
      (select-window target-window))))

(defun window-cmds-split-vertically (&optional arg)
  "When splitting window, show the other buffer in the new window.

With prefix arg ARG, don't select the new window."
  (interactive "P")
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer))
    (unless arg
      (select-window target-window))))

(defun window-cmds-toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(provide 'window-cmds)

;;; window-cmds.el ends here
