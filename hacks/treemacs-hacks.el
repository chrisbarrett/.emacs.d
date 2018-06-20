;;; treemacs-hacks.el --- Hacks for treemacs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'el-patch)

(el-patch-feature treemacs)



;; Fix errors caused by undefined text property.

(with-eval-after-load 'treemacs
  (with-no-warnings
    (el-patch-defun treemacs--pulse-png-advice (&rest _)
      "Make sure icons' background are pusled alongside the entire line."
      (el-patch-remove
        (when (eq 'treemacs-mode major-mode)
          (treemacs-with-writable-buffer
           (-when-let- [btn (treemacs-current-button)]
             (-let*- [(start (- (button-start btn) 2) )
                      (end (1+ start))
                      (img (get-text-property start 'display))
                      (cp (copy-sequence img))]
               (treemacs--set-img-property cp :background
                                           (face-attribute
                                            (overlay-get pulse-momentary-overlay 'face)
                                            :background nil t))
               (put-text-property start end 'display cp)))))))))

(provide 'treemacs-hacks)

;;; treemacs-hacks.el ends here
