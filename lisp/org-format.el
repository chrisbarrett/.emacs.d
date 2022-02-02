;;; org-format-headings.el --- Format orgmode headings in the current buffer.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Adapted from: https://emacs.stackexchange.com/a/28268

;;; Code:

(require 'org)

(defgroup org-format nil
  "Automatically format org buffers on save."
  :group 'productivity
  :prefix "org-format-")

(defcustom org-format-blank-lines-before-heading 1
  "Number of blank lines between a heading and preceding content"
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-content 0
  "Number of blank lines after the heading line and any property drawers."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-meta 0
  "Number of blank lines between headers and subsequent planning & drawers."
  :group 'org-format
  :type 'integer)

(defun org-format-all-headings ()
  "Ensure that blank lines exist between headings and their contents."
  (interactive)
  (let ((scope (if (equal (buffer-name) "archive.org") 'tree 'file)))
    (org-map-entries (lambda ()
                       ;; Widen so we can see space preceding the current
                       ;; headline.
                       (org-with-wide-buffer
                        (ensure-empty-lines org-format-blank-lines-before-heading))

                       (forward-line)
                       (unless (org-at-heading-p)
                         (ensure-empty-lines org-format-blank-lines-before-meta)

                         (org-end-of-meta-data t)
                         (ensure-empty-lines org-format-blank-lines-before-content)))
                     t
                     scope)))

(define-minor-mode org-format-on-save-mode
  "Minor mode to enable formatting on buffer save in org-mode."
  :lighter nil
  (cond
   (org-format-on-save-mode
    (add-hook 'before-save-hook 'org-format-all-headings nil t))
   (t
    (remove-hook 'before-save-hook 'org-format-all-headings t))))

(provide 'org-format-headings)

;;; org-format-headings.el ends here
