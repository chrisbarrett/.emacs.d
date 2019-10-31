;;; mu4e-hacks.el --- Hacks for mu4e.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature mu4e)

(cl-eval-when (compile)
  (require 'mu4e))

(with-eval-after-load 'mu4e
  (el-patch-defun mu4e~main-view ()
    "Create the mu4e main-view, and switch to it."
    (if (eq mu4e-split-view 'single-window)
        (if (buffer-live-p (mu4e-get-headers-buffer))
            (switch-to-buffer (mu4e-get-headers-buffer))
          (mu4e~main-menu))
      (mu4e~main-view-real nil nil)
      ((el-patch-swap switch-to-buffer display-buffer) mu4e~main-buffer-name)
      (goto-char (point-min)))
    (add-to-list 'global-mode-string '(:eval (mu4e-context-label)))))

(provide 'mu4e-hacks)

;;; mu4e-hacks.el ends here
