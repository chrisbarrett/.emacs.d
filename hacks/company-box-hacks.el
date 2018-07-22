;;; company-box-hacks.el --- Hacks for company-box.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature company-box)


;; Fix background colour on theme change.

(with-eval-after-load 'company-box
  (el-patch-defun company-box--display (string)
    "Display the completions."
    (company-box--render-buffer string)
    (unless (company-box--get-frame)
      (company-box--set-frame (company-box--make-frame)))
    (el-patch-add
      (with-selected-frame (company-box--get-frame)
        (set-background-color (face-background 'company-box-background nil t))))
    (company-box--set-frame-position (company-box--get-frame))
    (unless (frame-visible-p (company-box--get-frame))
      (make-frame-visible (company-box--get-frame)))
    (company-box--update-scrollbar (company-box--get-frame) t)))


;; Fix the foreground and background colours of documentation popup frames.

(with-eval-after-load 'company-box-doc

  (el-patch-defun company-box-doc--show (selection frame)
    (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                   (company-box--get-frame)
                                   (frame-visible-p (company-box--get-frame))))
                 (candidate (nth selection company-candidates))
                 (doc (or (company-call-backend 'quickhelp-string candidate)
                          (company-box-doc--fetch-doc-buffer candidate)))
                 (doc (company-box-doc--make-buffer doc)))
      (unless (frame-live-p (frame-parameter nil 'company-box-doc-frame))
        (set-frame-parameter nil 'company-box-doc-frame (company-box-doc--make-frame doc)))
      (el-patch-add
        (with-selected-frame (frame-parameter nil 'company-box-doc-frame)
          (set-foreground-color (face-foreground 'company-box-candidate nil t))
          (set-background-color (face-background 'company-box-background nil t))))
      (company-box-doc--set-frame-position (frame-parameter nil 'company-box-doc-frame))
      (unless (frame-visible-p (frame-parameter nil 'company-box-doc-frame))
        (make-frame-visible (frame-parameter nil 'company-box-doc-frame))))))

(provide 'company-box-hacks)

;;; company-box-hacks.el ends here
