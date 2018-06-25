;;; window-cmds.el --- Window management commands.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Window management commands from Steve Purcell's config.
;;
;; https://github.com/purcell/emacs.d/blob/80011b6/lisp/init-windows.el

;;; Code:

(require 'dash)

(autoload 'projectile-find-implementation-or-test "projectile")


;; Splitting commands

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


;; Layout commands

(defun window-cmds--tile-buffers (buf &rest bufs)
  (delete-other-windows)
  (-let [(fst . rest) (nreverse (cons buf bufs))]
    (switch-to-buffer fst)
    (dolist (buf rest)
      (split-window-right) (switch-to-buffer buf)))
  (balance-windows))

(defun window-cmds--buffers-for-focus ()
  (->>
   (with-current-buffer (-first #'buffer-file-name (buffer-list))
     (list (current-buffer)
           (find-file-noselect (projectile-find-implementation-or-test (buffer-file-name)))
           (get-buffer "*projectile-test*")))
   (-uniq)
   (-non-nil)))

(defvar window-cmds--window-configuration nil)

(defun window-cmds--buffers-in-window-configuration (config)
  (when config
    (save-window-excursion
      (ignore-errors
        (set-window-configuration config)
        (-map #'window-buffer (window-list))))))

(defun window-cmds--visible-buffers ()
  (-uniq (-map #'window-buffer (window-list))))

(defun window-cmds-tile ()
  "Toggle between tiled window layouts."
  (interactive)
  (let* ((current (window-cmds--visible-buffers))
         (next (window-cmds--buffers-for-focus)))
    (cond
     ((-same-items-p current next)
      (setq window-cmds--window-configuration (current-window-configuration))
      (window-cmds--tile-buffers (car current))
      (message "Focusing on buffer"))

     ((-contains? (window-cmds--buffers-in-window-configuration window-cmds--window-configuration)
                  (current-buffer))
      (set-window-configuration window-cmds--window-configuration)
      (setq window-cmds--window-configuration nil)
      (message "Restoring window configuration"))

     (t
      (setq window-cmds--window-configuration nil)
      (apply #'window-cmds--tile-buffers next)
      (message "Tiling %s buffer%s" (length next) (if (= 1 (length next)) "" "s"))))))

(provide 'window-cmds)

;;; window-cmds.el ends here
