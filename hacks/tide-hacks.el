;;; tide-hacks.el --- Patches for tide  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'el-patch)

(el-patch-feature tide)



(with-eval-after-load 'tide
  (with-no-warnings
    (el-patch-defun tide-documentation-at-point ()
      "Show documentation of the symbol at point."
      (interactive)
      (tide-command:quickinfo
       (tide-on-response-success-callback response
           (-if-let (buffer (tide-construct-documentation (plist-get response :body)))
               (progn
                 (el-patch-add (progn
                                 (with-current-buffer buffer
                                   (help-mode))
                                 (pop-to-buffer buffer t)))
                 (display-buffer buffer t)
                 (el-patch-swap
                   (if help-window-select
                       (progn
                         (pop-to-buffer buffer)
                         (message "Type \"q\" to restore previous buffer"))
                     (message "Type \"q\" in the documentation buffer to close it"))
                   (when help-window-select
                     (pop-to-buffer buffer))))
             (message "No documentation available.")))))))

(provide 'tide-hacks)

;;; tide-hacks.el ends here
