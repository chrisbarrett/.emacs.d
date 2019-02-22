;;; org-hacks.el --- Hacks for orgmode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature org-drill)



;; Fix incompatibility with org 9.2.

(with-eval-after-load 'org-drill
  (with-no-warnings
    (el-patch-defun org-drill-hide-subheadings-if (test)
      "TEST is a function taking no arguments. TEST will be called for each
of the immediate subheadings of the current drill item, with the point
on the relevant subheading. TEST should return nil if the subheading is
to be revealed, non-nil if it is to be hidden.
Returns a list containing the position of each immediate subheading of
the current topic."
      (let ((drill-entry-level (org-current-level))
            (drill-sections nil))
        (org-show-subtree)
        (save-excursion
          (org-map-entries
           (lambda ()
             (when (and (not (org-invisible-p))
                        (> (org-current-level) drill-entry-level))
               (when (or (/= (org-current-level) (1+ drill-entry-level))
                         (funcall test))
                 (hide-subtree))
               (push (point) drill-sections)))
           (el-patch-swap "" t) 'tree))
        (reverse drill-sections)))))



(provide 'org-hacks)

;;; org-hacks.el ends here
