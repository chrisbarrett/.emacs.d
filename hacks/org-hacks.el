;;; org-hacks.el --- Hacks for org-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(defvar org-hacks--bullet (concat "⦿"))

(el-patch-feature org)

(with-eval-after-load 'org
  (with-no-warnings
    (el-patch-defun org-font-lock-add-priority-faces (limit)
      "Add the special priority faces."
      (while (re-search-forward "^\\*+ .*?\\(\\[#\\(.\\)\\]\\)" limit t)
        (add-text-properties
         (match-beginning 1) (match-end 1)
         (list 'face (org-get-priority-face (string-to-char (match-string 2)))
               'font-lock-fontified t
               (el-patch-add 'display org-hacks--bullet)))))))

(el-patch-feature org-agenda)

(with-eval-after-load 'org-agenda
  (with-no-warnings
    (el-patch-defun org-agenda-fontify-priorities ()
      "Make highest priority lines bold, and lowest italic."
      (interactive)
      (mapc (lambda (o) (when (eq (overlay-get o 'org-type) 'org-priority)
                     (delete-overlay o)))
            (overlays-in (point-min) (point-max)))
      (save-excursion
        (let (b e p ov h l)
          (goto-char (point-min))
          (while (re-search-forward "\\[#\\(.\\)\\]" nil t)
            (setq h (or (get-char-property (point) 'org-highest-priority)
                        org-highest-priority)
                  l (or (get-char-property (point) 'org-lowest-priority)
                        org-lowest-priority)
                  p (string-to-char (match-string 1))
                  b (match-beginning 0)
                  e (if (eq org-agenda-fontify-priorities 'cookies)
                        (match-end 0)
                      (point-at-eol))
                  ov (make-overlay b e))
            (overlay-put
             ov 'face
             (let ((special-face
                    (cond ((org-face-from-face-or-color
                            'priority 'org-priority
                            (cdr (assoc p org-priority-faces))))
                          ((and (listp org-agenda-fontify-priorities)
                                (org-face-from-face-or-color
                                 'priority 'org-priority
                                 (cdr (assoc p org-agenda-fontify-priorities)))))
                          ((equal p l) 'italic)
                          ((equal p h) 'bold))))
               (if special-face (list special-face 'org-priority) 'org-priority)))
            (el-patch-add (overlay-put ov 'display org-hacks--bullet))
            (overlay-put ov 'org-type 'org-priority)))))))

(provide 'org-hacks)

;;; org-hacks.el ends here
