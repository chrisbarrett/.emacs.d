;;; pass-hacks.el --- Hacks for pass  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature pass)



;; Don't prompt for confirmation when quitting.
(with-eval-after-load 'pass
  (el-patch-defun pass-quit ()
    "Kill the buffer quitting the window."
    (interactive)
    (el-patch-splice 2 0 ;; remove confirmation prompt
      (when (y-or-n-p "Kill all pass entry buffers? ")
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (eq major-mode 'pass-view-mode)
              (kill-buffer buf))))))
    (quit-window t)))

(provide 'pass-hacks)

;;; pass-hacks.el ends here
