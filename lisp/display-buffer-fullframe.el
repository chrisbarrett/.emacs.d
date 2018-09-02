;;; display-buffer-fullframe.el --- Command to display a buffer in a dedicated frame.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(defun display-buffer-fullframe (buffer alist)
  (when-let* ((window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-pop-up-window buffer alist)
                          (display-buffer-use-some-window buffer alist))))
    (delete-other-windows window)
    window))

(provide 'display-buffer-fullframe)

;;; display-buffer-fullframe.el ends here
