;;; org-edna-hacks.el --- Hacks for org-edna.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(cl-eval-when (compile)
  (require 'org-edna))

(el-patch-feature org-edna)

;; Fix flaky regexes.

(with-eval-after-load 'org-edna
  (el-patch-defun org-edna-edit-blocker-section-text ()
    "Collect the BLOCKER section text from an edit buffer."
    (when (org-edna-in-edit-buffer-p)
      (let ((original-text (org-edna-edit-text-between-markers
                            org-edna-blocker-section-marker
                            org-edna-trigger-section-marker)))
        ;; Strip the BLOCKER key
        (when (string-match (el-patch-swap "^BLOCKER\n\\(\\(?:.*\n\\)+\\)"
                                           (rx bol "BLOCKER" (+ "\n") (group (+ nonl))))
                            original-text)
          (org-edna-replace-newlines (match-string 1 original-text))))))

  (el-patch-defun org-edna-edit-trigger-section-text ()
    "Collect the TRIGGER section text from an edit buffer."
    (when (org-edna-in-edit-buffer-p)
      (let ((original-text (org-edna-edit-text-between-markers
                            org-edna-trigger-section-marker
                            (point-max-marker))))
        (when (string-match (el-patch-swap "^TRIGGER\n\\(\\(?:.*\n\\)+\\)"
                                           (rx bol "TRIGGER" (+ "\n") (group (+ nonl))))
                            original-text)
          (org-edna-replace-newlines (match-string 1 original-text)))))))

(provide 'org-edna-hacks)

;;; org-edna-hacks.el ends here
