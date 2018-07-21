;;; omnibox-hacks.el --- Patches for omnibox.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)
(require 'dash)

(el-patch-feature 'omnibox)



;; Allow the background colour to be themed.
(with-eval-after-load 'omnibox
  (defvar omnibox-frame-parameters)

  (el-patch-defun omnibox--make-frame (buffer)
    (-if-let* ((frame (omnibox--get frame)))
        (progn
          (el-patch-add
            (with-selected-frame frame
              (set-background-color (face-background 'omnibox-background nil t))))
          (omnibox--update-input-buffer (omnibox--get input))
          (make-frame-visible frame)
          (redisplay))
      (let* ((before-make-frame-hook nil)
             (after-make-frame-functions nil)
             (internal-border (round (* (frame-char-width) 1.2)))
             (x (- (/ (frame-pixel-width) 2)
                   (/ (* 90 (frame-char-width)) 2)
                   internal-border))
             (frame (make-frame
                     (append `((el-patch-add
                                 (background-color . ,(face-background 'omnibox-background nil t)))
                               (left . ,x)
                               (internal-border-width . ,internal-border)
                               (default-minibuffer-frame . ,(selected-frame))
                               (minibuffer . ,(minibuffer-window))
                               (parent-frame . ,(selected-frame)))
                             omnibox-frame-parameters)))
             (window (frame-selected-window frame)))
        (set-window-buffer window buffer)
        (redirect-frame-focus frame (selected-frame))
        (set-window-dedicated-p window t)
        (omnibox--set frame frame)
        (with-selected-frame frame
          (display-buffer-in-side-window
           (omnibox--update-input-buffer (omnibox--get input))
           '((side . top) (window-height . 1))))
        frame))))



(provide 'omnibox-hacks)

;;; omnibox-hacks.el ends here
