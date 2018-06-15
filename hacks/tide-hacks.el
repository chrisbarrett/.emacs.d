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
               (el-patch-swap (display-buffer buffer t)
                              (progn
                                (with-current-buffer buffer
                                  (help-mode))
                                (pop-to-buffer buffer t)))
             (message "No documentation available.")))))))

(provide 'tide-hacks)

;;; tide-hacks.el ends here
