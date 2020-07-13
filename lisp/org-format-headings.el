;;; org-format-headings.el --- Format orgmode headings in the current buffer.  -*- lexical-binding: t; -*-

;; Author: John Kitchin

;;; Commentary:

;; https://emacs.stackexchange.com/a/28268

;;; Code:

(require 'cl-lib)

(require 'org)

(defvar org-blank-lines-after-heading 1
  "Number of blank lines to separate a heading from the content.")

(defvar org-blank-lines-after-content '(1 . 1)
  "Cons cell for the number of blank lines after content in a heading.
The car is for when the next heading is at the same level, and
the cdr is for when the next heading is at a different level.
This is for the body specific to the headline, not counting
subheadings.")

(defun org-format-heading ()
  "Insert and remove whitespace to satisfy orgmode buffer format.

Make sure each headline has exactly
`org-blank-lines-after-heading' after the heading, and
`org-blank-lines-after-content' blank lines at the end of its
content. Only works when point is in a headline."
  (when (org-at-heading-p)
    (let ((current-level (nth 0 (org-heading-components)))
          next-level)
      (save-excursion
        (org-end-of-meta-data)
        ;; chomp blank lines then add what you want back.
        (while (and (not (eobp)) (looking-at "^[[:space:]]*$"))
          (kill-line))
        (insert (cl-loop for i from 0 below org-blank-lines-after-heading concat "\n")))

      ;; Now go to the end of content and insert lines if needed.
      (save-excursion
        (when (outline-next-heading)
          (setq next-level (nth 0 (org-heading-components)))
          ;; chomp lines back then reinsert them.
          (forward-line -1)
          (while (looking-at "^[[:space:]]*$")
            (kill-line)
            (forward-line -1))
          (unless (eobp) (end-of-line))
          (insert (cl-loop for i from 0 below (if (= current-level next-level)
                                                  (car org-blank-lines-after-content)
                                                (cdr org-blank-lines-after-content))
                           concat "\n")))))))

(defun org-format-all-headings ()
  "Insert and remove whitespace to satisfy orgmode buffer format."
  (interactive)
  (save-excursion
    (org-save-outline-visibility t
      (org-cycle '(64))
      (message nil)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (org-format-heading)))))

(provide 'org-format-headings)

;;; org-format-headings.el ends here
