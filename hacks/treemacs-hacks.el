;;; treemacs-hacks.el --- Hacks for treemacs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'el-patch)
(require 'paths)

(el-patch-feature treemacs)
(el-patch-feature treemacs-persistence)



;; Error persist file is set as defconst and referenced during load. Fix value.

(with-eval-after-load 'treemacs-persistence
  (el-patch-defconst treemacs--last-error-persist-file
    (el-patch-swap
      (f-join user-emacs-directory ".cache" "treemacs-persist-at-last-error")
      (f-join paths-cache-directory "treemacs" "treemacs-errors"))
    "File that stores the treemacs state as it was during the last load error."))

;; Fix errors caused by undefined text property.

(with-eval-after-load 'treemacs
  (with-no-warnings
    (el-patch-defun treemacs--pulse-png-advice (&rest _)
      "Make sure icons' background are pusled alongside the entire line."
      (el-patch-remove
        (when (eq 'treemacs-mode major-mode)
          (treemacs-with-writable-buffer
           (-when-let (btn (treemacs-current-button))
             (let* ((start (max (point-at-bol) (- (button-start btn) 2)) )
                    (end (1+ start))
                    (img (get-text-property start 'display))
                    (cp (copy-sequence img)))
               (treemacs--set-img-property cp :background
                                           (face-attribute
                                            (overlay-get pulse-momentary-overlay 'face)
                                            :background nil t))
               (put-text-property start end 'display cp)))))))))

(provide 'treemacs-hacks)

;;; treemacs-hacks.el ends here
