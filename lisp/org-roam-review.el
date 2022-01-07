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

(defgroup org-roam-review nil
  "Extends org-roam with spaced-repetition review of notes."
  :group 'productivity
  :prefix "org-roam-review-")

(defcustom org-roam-review-cache-file "~/org/.org-roam-review"
  "Location of the cache file for quickly finding review files."
  :group 'org-roam-review
  :type 'file)

(defconst org-roam-review-maturity-values '("seedling" "evergreen" "budding"))


;;; Define cache operations

;; Maintain a cache file to ensure review sessions are as responsive as
;; possible.

;; Define plumbing commands for cache here.

(defvar org-roam-review--cache nil)

(defun org-roam-review--cache ()
  (unless org-roam-review--cache
    (setq org-roam-review--cache
          (or (ignore-errors (read (f-read-text org-roam-review-cache-file)))
              (make-hash-table :test #'equal))))
  org-roam-review--cache)

(defun org-roam-review--cache-mutate (fn)
  (let ((cache (org-roam-review--cache)))
    (funcall fn cache)
    (f-write-text (prin1-to-string cache t) 'utf-8 org-roam-review-cache-file)
    cache))

(defun org-roam-review--cache-clear ()
  (setq org-roam-review--cache nil)
  (when (file-exists-p org-roam-review-cache-file)
    (delete-file org-roam-review-cache-file)))

;; Define cache-management porcelain in terms of plumbing.

(defun org-roam-review--props-in-buffer ()
  (org-with-wide-buffer
   (save-match-data
     (goto-char (point-min))
     (let ((acc))
       (while (search-forward-regexp (org-re-property "ID") nil t)
         (let ((id (match-string-no-properties 3))
               (maturity (org-entry-get (point) "MATURITY"))
               (last-review (org-entry-get (point) "LAST_REVIEW"))
               (next-review (org-entry-get (point) "NEXT_REVIEW")))
           (push (list :id id :last-review last-review :next-review next-review :maturity maturity) acc)))
       (nreverse acc)))))

(defun org-roam-review--update-by-props-in-buffer (cache)
  (dolist (info (org-roam-review--props-in-buffer))
    (-let [(&plist :id) info]
      (puthash id info cache))))

(defun org-roam-review--cache-update ()
  "Update the evergreen notes cache from `after-save-hook'."
  (when (and (derived-mode-p 'org-mode)
             (not (org-roam-dailies--daily-note-p (buffer-file-name))))
    (org-roam-review--cache-mutate #'org-roam-review--update-by-props-in-buffer)))

(defun org-roam-review--cache-collect (fn)
  (let ((table (copy-hash-table (org-roam-review--cache))))
    (maphash (lambda (key value)
               (if-let* ((result (funcall fn key value)))
                   (puthash key result table)
                 (remhash key table)))
             table)
    table))

;;;###autoload
(defun org-roam-review-cache-rebuild ()
  "Rebuild the evergreen notes cache."
  (interactive)
  (org-roam-review--cache-clear)
  (f-files org-roam-directory
           (lambda (file)
             (with-temp-buffer
               (insert-file-contents file)
               (unless (org-roam-dailies--daily-note-p file)
                 (org-roam-review--cache-mutate #'org-roam-review--update-by-props-in-buffer))))
           t)
  (message "Rebuilt evergreen notes index."))

;;;###autoload
(defun org-roam-review-cache-show ()
  "Show the contents the evegreen notes cache for debugging."
  (interactive)
  (pp-display-expression (org-roam-review--cache)
                         "*org-roam-review cache*"))


;;; Review buffers

(define-derived-mode org-roam-review-mode org-roam-mode "Org-roam-review"
  "Major mode for displaying relevant information about Org-roam
nodes for review."
  :group 'org-roam-review)

(defun org-roam-review--insert-review-section (node)
  (magit-insert-section section (org-roam-node-section)
    (insert (propertize (org-roam-node-title node)
                        'font-lock-face 'org-roam-title))
    (magit-insert-heading)
    (oset section node node)
    (magit-insert-section section (org-roam-preview-section)
      (insert (org-roam-fontify-like-in-org-mode
               (org-roam-preview-get-contents (org-roam-node-file node) 0))
              "\n")
      (oset section file (org-roam-node-file node))
      (oset section point 0)
      (insert ?\n))))

(defun org-roam-review--create-review-buffer (title instructions filtered-cache)
  (let ((buf (get-buffer-create "*org-roam-review*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-roam-review-mode)
        (org-roam-buffer-set-header-line-format title)

        (cond ((hash-table-empty-p filtered-cache)
               (insert (concat (propertize "You're up-to-date!" 'face 'font-lock-comment-face) " 😸"))
               (newline))
              (t
               (when instructions
                 (magit-insert-section (org-roam-review-instructions)
                   (let ((start (point)))
                     (insert instructions)
                     (fill-region start (point)))
                   (newline 2)))

               (magit-insert-section (org-roam-review)
                 (magit-insert-heading)
                 (maphash (-lambda (_ (&plist :id))
                            (when-let* ((node (org-roam-node-from-id id)))
                              (org-roam-review--insert-review-section node)))
                          filtered-cache))
               (goto-char 0)
               (let ((children (oref magit-root-section children)))
                 (mapc 'magit-section-hide children))))))

    (display-buffer buf)))

;;;###autoload
(defun org-roam-review ()
  "List notes that are due for review."
  (interactive)
  (org-roam-review--create-review-buffer
   "Due Notes"
   "The notes below are due for review.
Read each note and add new thoughts and connections,
then mark them as reviewed by setting their maturity."
   (org-roam-review--cache-collect
    (-lambda (_ (value &as &plist :next-review))
      (when next-review
        (when (org-time-less-p (org-parse-time-string next-review) nil)
          value))))))

;;;###autoload
(defun org-roam-review-pending ()
  "List notes missing required properties to be used for reviews.

This is useful for migrating notes into the spaced repetition
system."
  (interactive)
  (org-roam-review--create-review-buffer
   "Pending Notes"
   "The notes below are missing the properties required for reviews.
Read each note in the list and set their maturity."
   (org-roam-review--cache-collect
    (-lambda (_ (value &as &plist :maturity :next-review))
      (unless (and next-review maturity)
        value)))))



(defun org-roam-review--update-next-review (&optional quality)
  "Adapted from org-drill.

- only use sm5 algorithm for simplicity
- use properties instead of SCHEDULED.
- remove support for 'weighting' a note."
  (-let* ((quality (or quality 4))
          (ofmatrix org-drill-sm5-optimal-factor-matrix)
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

(defun org-roam-review--update-note (maturity)
  (cl-assert (member maturity org-roam-review-maturity-values))
  (atomic-change-group
    (org-with-wide-buffer
     (when (org-roam-dailies--daily-note-p)
       (user-error "Cannot set maturity on daily file"))

     (if-let* ((id (org-entry-get (point) "ID" t)))
         (org-id-goto id)
       (error "No ID property for tree at point"))

     (org-set-property "MATURITY" maturity)
     (org-set-property "LAST_REVIEW" (org-format-time-string "[%Y-%m-%d %a]"))

     (let ((next-review (org-roam-review--update-next-review)))
       (ignore-errors
         (org-roam-tag-remove org-roam-review-maturity-values))
       (org-roam-tag-add (list maturity))

       (message "Maturity set to '%s'. Review scheduled for %s" maturity next-review)))))

;;;###autoload
(defun org-roam-review-set-budding ()
  "Set the current note as a 'budding' note."
  (interactive)
  (org-roam-review--update-note "budding"))

;;;###autoload
(defun org-roam-review-set-seedling ()
  "Set the current note as a 'seedling' note."
  (interactive)
  (org-roam-review--update-note "seedling"))

;;;###autoload
(defun org-roam-review-set-evergreen ()
  "Set the current note as a 'evergreen' note."
  (interactive)
  (org-roam-review--update-note "evergreen"))



;;;###autoload
(define-minor-mode org-roam-review-cache-mode
  "Minor mode to enable book-keeping used for notes reviews"
  :group 'org-roam-review
  (if org-roam-review-cache-mode
      (add-hook 'after-save-hook #'org-roam-review--cache-update nil t)
    (remove-hook 'after-save-hook #'org-roam-review--cache-update t)))

(provide 'org-roam-review)

;;; org-roam-review.el ends here
