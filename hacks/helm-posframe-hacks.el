;;; helm-posframe-hacks.el --- Hacks for helm-posframe.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(with-eval-after-load 'helm-posframe
  (with-no-warnings
    (el-patch-defun helm-posframe-display (buffer &optional _resume)
      "The display function which is used by `helm-display-function'.
Argument BUFFER."
      (setq helm-posframe-buffer buffer)
      (posframe-show
       buffer
       :position (point)
       :poshandler helm-posframe-poshandler
       :width (or helm-posframe-width (+ (window-width) 2))
       :height (or helm-posframe-height helm-display-buffer-height)
       :min-height 10
       :min-width 50
       :font helm-posframe-font
       (el-patch-add
         :internal-border-width 10
         :background-color (face-background 'ivy-posframe nil t))
       :override-parameters helm-posframe-parameters
       :respect-header-line t))))


(provide 'helm-posframe-hacks)

;;; helm-posframe-hacks.el ends here
