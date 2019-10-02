;;; ivy-filthy-rich-hacks.el --- Hacks for ivy-filthy-rich
;;; Commentary:
;;; Code:

(require 'el-patch)

(cl-eval-when (compile)
  (require 'ivy-filthy-rich)
  (require 'ivy-posframe nil t))

(el-patch-feature ivy-filthy-rich)

(with-eval-after-load 'ivy-filthy-rich
  (el-patch-defun ivy-filthy-rich--format-candidate (candidate format)
    "Format CANDIDATE into a rich candidate according to FORMAT."
    ;; 1. replace functions with actual info string
    (let ((info-list ())
          (entry-sequence ())
          (format (copy-tree format))
          (ivy-filthy-rich-max-length (when (equal 0 ivy-filthy-rich-max-length)
                                        (1- (el-patch-swap (frame-width)
                                                           (if (bound-and-true-p ivy-posframe-mode)
                                                               (plist-get (funcall ivy-posframe-size-function)
                                                                          :min-width)
                                                             (frame-width)))))))
      (when (sequencep candidate)
        (setq candidate (substring-no-properties candidate)))
      (dolist (format-element format)
        (let ((func (alist-get 'value format-element)))
          ;; evaluate the function and replace it with returned value list
          (setf (alist-get 'value format-element) (funcall func candidate))
          ;; add the modified entry to new list
          (add-to-list 'info-list format-element t)))
      ;; 2. trim each part(info) of entry to it's planned max length (* prop ivy-filthy-rich-max-length)
      (setq info-list (ivy-filthy-rich--trim-entry-to-max-length info-list))
      ;; 3. format info-list into a sequence of strings to be concated
      ;; 4. sequence to string
      ;; each info is an alist with key: value, prop, etc
      (apply 'ivy-filthy-rich--concat-entry-sequence (ivy-filthy-rich--format-to-sequence info-list)))))

(provide 'ivy-filthy-rich-hacks)

;;; ivy-filthy-rich-hacks.el ends here
