;;; org-roam-review.el --- Extends org-roam with spaced-repetition review of notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'dash)
(require 'org)
(require 'org-drill)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'f)
(require 'ht)

(defgroup org-roam-review nil
  "Extends org-roam with spaced-repetition review of notes."
  :group 'productivity
  :prefix "org-roam-review-")

(defcustom org-roam-review-cache-file "~/org/.org-roam-review"
  "Location of the cache file for quickly finding review files."
  :group 'org-roam-review
  :type 'file)

(defconst org-roam-review-maturity-values '("seedling" "evergreen" "budding"))

(defcustom org-roam-review-ignored-tags '()
  "A list of tags that define a note should not be considered a
candidate for reviews."
  :group 'org-roam-review
  :type '(list string))


;;; Cached note type & accessors

(cl-defstruct org-roam-review-note
  "A cached org-roam note for use in the review buffer."
  id tags next-review last-review maturity)

(defun org-roam-review-note-ignored-p (note)
  (seq-intersection (org-roam-review-note-tags note) org-roam-review-ignored-tags))

(defun org-roam-review-note-due-p (note)
  (when-let* ((next-review (org-roam-review-note-next-review note)))
    (or (time-equal-p next-review nil)
        (time-less-p next-review nil))))


;;; Define cache operations

;; Maintain a cache file to ensure review sessions are as responsive as
;; possible.

;; Define plumbing commands for cache here.

(defvar org-roam-review--cache nil)

(defun org-roam-review--cache ()
  (unless org-roam-review--cache
    (setq org-roam-review--cache
          (or (ignore-errors (ht-from-alist (read (f-read-text org-roam-review-cache-file))))
              (make-hash-table :test #'equal))))
  org-roam-review--cache)

(defun org-roam-review--cache-mutate (fn)
  (let ((cache (org-roam-review--cache)))
    (funcall fn cache)
    (f-write-text (prin1-to-string (ht-to-alist cache)) 'utf-8 org-roam-review-cache-file)
    cache))

(defun org-roam-review--cache-clear ()
  (setq org-roam-review--cache nil)
  (when (file-exists-p org-roam-review-cache-file)
    (delete-file org-roam-review-cache-file)))

;; Define cache-management porcelain in terms of plumbing.

(defun org-roam-review-notes-from-this-buffer ()
  (org-with-wide-buffer
   (save-match-data
     (goto-char (point-min))
     (let ((acc))
       (while (search-forward-regexp (org-re-property "ID") nil t)
         (let* ((id (match-string-no-properties 3))
                (item (make-org-roam-review-note
                       :id id
                       :next-review (-some->> (org-entry-get (point) "NEXT_REVIEW")
                                      (org-parse-time-string)
                                      (encode-time))
                       :last-review (-some->> (org-entry-get (point) "LAST_REVIEW")
                                      (org-parse-time-string)
                                      (encode-time))
                       :maturity (org-entry-get (point) "MATURITY")
                       :tags (org-roam-review--file-or-headline-tags))))
           (push item acc)))
       (nreverse acc)))))

(defun org-roam-review--update-by-props-in-buffer (cache)
  (dolist (note (org-roam-review-notes-from-this-buffer))
    (puthash (org-roam-review-note-id note) note cache)))

(defun org-roam-review--cache-update ()
  "Update the evergreen notes cache from `after-save-hook'."
  (when (and (derived-mode-p 'org-mode)
             (not (org-roam-dailies--daily-note-p (buffer-file-name))))
    (org-roam-review--cache-mutate #'org-roam-review--update-by-props-in-buffer)))

(defun org-roam-review--cache-collect (fn)
  (let ((table (copy-hash-table (org-roam-review--cache))))
    (maphash (lambda (key note)
               (if-let* ((result (funcall fn note)))
                   (puthash key result table)
                 (remhash key table)))
             table)
    (hash-table-values table)))

(defun org-roam-review--file-or-headline-tags ()
  (if (org-before-first-heading-p)
      org-file-tags
    (org-get-tags)))

(defun org-roam-review--cache-roam-files ()
  (f-files org-roam-directory
           (lambda (file)
             (with-temp-buffer
               (insert-file-contents file)
               (setq-local major-mode 'org-mode)
               (org-set-regexps-and-options)
               (let ((skip-p (or (org-roam-dailies--daily-note-p file)
                                 (seq-intersection org-roam-review-ignored-tags (org-roam-review--file-or-headline-tags)))))
                 (unless skip-p
                   (org-roam-review--cache-mutate #'org-roam-review--update-by-props-in-buffer)))))
           t))

;;;###autoload
(defun org-roam-review-cache-rebuild ()
  "Rebuild the evergreen notes cache."
  (interactive)
  (org-roam-review--cache-clear)
  (org-roam-review--cache-roam-files)
  ;; Write back to disk.
  (org-roam-review--cache-mutate #'ignore)
  (message "Rebuilt evergreen notes index."))

;;;###autoload
(defun org-roam-review-cache-show ()
  "Show the contents the evegreen notes cache for debugging."
  (interactive)
  (pp-display-expression (org-roam-review--cache)
                         "*org-roam-review cache*"))


;;; Review buffers

(defvar-local org-roam-review-buffer-refresh-command nil)

(defun org-roam-review-refresh ()
  "Rebuild the review buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*org-roam-review*")
    (unless org-roam-review-buffer-refresh-command
      (error "Refresh command not defined"))
    (call-interactively org-roam-review-buffer-refresh-command))
  (message "Buffer refreshed"))

(defvar org-roam-review-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "TAB") #'magit-section-cycle)
    (define-key keymap (kbd "g") #'org-roam-review-refresh)
    keymap))

(define-derived-mode org-roam-review-mode org-roam-mode "Org-roam-review"
  "Major mode for displaying relevant information about Org-roam
nodes for review."
  :group 'org-roam-review)

(defun org-roam-review--insert-node (node)
  (magit-insert-section section (org-roam-node-section)
    (magit-insert-heading (propertize (org-roam-node-title node)
                                      'font-lock-face 'org-roam-title))
    (oset section node node)
    (magit-insert-section section (org-roam-preview-section)
      (insert (org-roam-fontify-like-in-org-mode
               (org-roam-preview-get-contents (org-roam-node-file node) 0)))
      (oset section file (org-roam-node-file node))
      (oset section point 0)
      (insert "\n\n"))))

(defvar org-roam-review-default-placeholder
  (propertize "(None)" 'face 'font-lock-comment-face))

(cl-defun org-roam-review--create-review-buffer (&key title instructions notes group-on refresh-command placeholder)
  (cl-assert (and title instructions refresh-command))
  (let ((buf (get-buffer-create "*org-roam-review*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-roam-review-mode)
        (org-roam-buffer-set-header-line-format title)
        (setq-local org-roam-review-buffer-refresh-command refresh-command)

        (cl-labels ((insert-notes (notes)
                                  (let ((section (magit-insert-section (org-roam-review)
                                                   (magit-insert-heading)
                                                   (mapc (lambda (note)
                                                           (-some->> (org-roam-review-note-id note)
                                                             (org-roam-node-from-id)
                                                             (org-roam-review--insert-node)))
                                                         notes))))
                                    (mapc #'magit-section-hide (oref section children)))))

          (when (and instructions notes)
            (magit-insert-section (org-roam-review-instructions)
              (let ((start (point)))
                (insert (propertize instructions 'font-lock-face 'font-lock-comment-face))
                (fill-region start (point)))
              (newline 2)))

          (cond ((null notes)
                 (insert (or placeholder org-roam-review-default-placeholder))
                 (newline))
                (group-on
                 (mapc (-lambda ((key . group))
                         (magit-insert-section (org-roam-review-note-group)
                           (magit-insert-heading (propertize key 'font-lock-face 'magit-section-heading))
                           (insert-notes group)
                           (insert "\n")))
                       (seq-group-by group-on notes)))
                (t
                 (insert-notes notes))))
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun org-roam-review (&optional all)
  "List notes that are due for review.

With optional prefix arg ALL, list all evergreen notes
categorised by their maturity."
  (interactive "P")
  (if all
      (org-roam-review-list-categorised)
    (org-roam-review-list-due)))

(defun org-roam-review--maturity-header-for-note (note)
  (pcase (org-roam-review-note-maturity note)
    ("budding" "Budding 🪴")
    ("seedling" "Seedling 🌱")
    ("evergreen" "Evergreen 🌲")
    (value value)))


;;;###autoload
(defun org-roam-review-list-due ()
  "List notes that are due for review."
  (interactive)
  (org-roam-review--create-review-buffer
   :title "Due Notes"
   :instructions "The notes below are due for review.
Read each note and add new thoughts and connections, then mark
them as reviewed with `org-roam-review-accept',
`org-roam-review-bury' or by updating their maturity."
   :placeholder (concat (propertize "You're up-to-date!" 'face 'font-lock-comment-face) " 😸")
   :refresh-command #'org-roam-review-list-due
   :group-on #'org-roam-review--maturity-header-for-note
   :notes (org-roam-review--cache-collect
           (lambda (note)
             (when (and (not (org-roam-review-note-ignored-p note))
                        (org-roam-review-note-due-p note))
               note)))))

;;;###autoload
(defun org-roam-review-list-categorised ()
  "List all evergreen notes categorised by maturity."
  (interactive)
  (org-roam-review--create-review-buffer
   :title "Evergreen Notes"
   :instructions "The notes below are categorised by maturity."
   :refresh-command #'org-roam-review-list-categorised
   :group-on #'org-roam-review--maturity-header-for-note
   :notes (org-roam-review--cache-collect
           (lambda (note)
             (when (and (not (org-roam-review-note-ignored-p note))
                        (org-roam-review-note-maturity note))
               note)))))

;;;###autoload
(defun org-roam-review-list-uncategorised ()
  "List notes missing required properties to be used for reviews.

This is useful for migrating notes into the spaced repetition
system."
  (interactive)
  (org-roam-review--create-review-buffer
   :title "Uncategorised Notes"
   :instructions "The notes below are missing the properties
needed to be included in reviews. Categorise them as appropriate."
   :refresh-command #'org-roam-review-list-uncategorised
   :notes (org-roam-review--cache-collect
           (lambda (note)
             (unless (or (org-roam-review-note-ignored-p note)
                         (org-roam-review-note-maturity note)
                         (org-roam-review-note-next-review note))
               note)))))



(defun org-roam-review--update-next-review (quality)
  "Adapted from org-drill.

- only use sm5 algorithm for simplicity
- use properties instead of SCHEDULED.
- remove support for 'weighting' a note."
  (-let* ((ofmatrix org-drill-sm5-optimal-factor-matrix)
          ((last-interval repetitions failures total-repeats meanq ease) (org-drill-get-item-data))
          ((next-interval repetitions ease failures meanq total-repeats new-ofmatrix)
           (org-drill-determine-next-interval-sm5 last-interval repetitions
                                                  ease quality failures
                                                  meanq total-repeats ofmatrix))
          (next-interval (if (cl-minusp next-interval)
                             next-interval
                           (max 1.0 (+ last-interval (- next-interval last-interval)))))
          (new-time (time-add (current-time) (days-to-time (round next-interval)))))
    (setq org-drill-sm5-optimal-factor-matrix new-ofmatrix)
    (org-drill-store-item-data next-interval repetitions failures total-repeats meanq ease)

    (let ((next-review (org-format-time-string "[%Y-%m-%d %a]" new-time)))
      (org-set-property "NEXT_REVIEW" next-review)
      next-review)))

(defun org-roam-review--update-note (maturity bury)
  (cl-assert (member maturity org-roam-review-maturity-values))
  (cl-assert (derived-mode-p 'org-mode))
  (atomic-change-group
    (org-with-wide-buffer
     (when (org-roam-dailies--daily-note-p)
       (user-error "Cannot set maturity on daily file"))

     (if-let* ((id (org-entry-get (point) "ID" t)))
         (org-id-goto id)
       (error "No ID property for tree at point"))

     (org-set-property "MATURITY" maturity)
     (org-set-property "LAST_REVIEW" (org-format-time-string "[%Y-%m-%d %a]"))

     (let* (
            ;; High score means the note appears less often--in
            ;; spaced-repetition learning, it's been 'learned'.
            (score (if bury 5 3))
            (next-review (org-roam-review--update-next-review score)))
       (ignore-errors
         (org-roam-tag-remove org-roam-review-maturity-values))
       (org-roam-tag-add (list maturity))

       (save-buffer)
       (message "Maturity set to '%s'. Review scheduled for %s" maturity next-review)))))

;;;###autoload
(defun org-roam-review-accept ()
  "Confirm review of the current note."
  (interactive)
  (let ((maturity (org-entry-get (point) "MATURITY")))
    (org-roam-review--update-note maturity nil)))

;;;###autoload
(defun org-roam-review-bury ()
  "Confirm review of the current note and bury it."
  (interactive)
  (let ((maturity (org-entry-get (point) "MATURITY")))
    (org-roam-review--update-note maturity 'bury)))

;;;###autoload
(defun org-roam-review-set-budding (&optional bury)
  "Set the current note as a 'budding' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (org-roam-review--update-note "budding" bury))

;;;###autoload
(defun org-roam-review-set-seedling (&optional bury)
  "Set the current note as a 'seedling' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (org-roam-review--update-note "seedling" bury))

;;;###autoload
(defun org-roam-review-set-evergreen (&optional bury)
  "Set the current note as an 'evergreen' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (org-roam-review--update-note "evergreen" bury))



;;;###autoload
(define-minor-mode org-roam-review-cache-mode
  "Minor mode to enable book-keeping used for notes reviews"
  :group 'org-roam-review
  (if org-roam-review-cache-mode
      (add-hook 'after-save-hook #'org-roam-review--cache-update nil t)
    (remove-hook 'after-save-hook #'org-roam-review--cache-update t)))

(provide 'org-roam-review)

;;; org-roam-review.el ends here
