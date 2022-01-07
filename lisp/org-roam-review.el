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

(require 'org)
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


;; We maintain a cache file for snappy reviews. Define plumbing commands for
;; cache here.

(defvar org-roam-review--cache nil)

(defun org-roam-review--cache ()
  (unless org-roam-review--cache
    (setq org-roam-review--cache
          (or (ignore-errors (read (f-read-text org-roam-review-cache-file)))
              (make-hash-table))))
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
               (review (org-entry-get (point) "REVIEW")))
           (push (list :id id :review review :maturity maturity) acc)))
       (nreverse acc)))))

(defun org-roam-review--update-by-props-in-buffer (cache)
  (dolist (info (org-roam-review--props-in-buffer))
    (-let [(&plist :id) info]
      (puthash id info cache))))

(defun org-roam-review--cache-update ()
  "Update the evergreen notes cache from `after-save-hook'."
  (when (and (derived-mode-p 'org-mode)
             (not (org-roam-dailies--daily-note-p (buffer-file-name))))
    (org-roam-review--cache-mutate #' org-roam-review--update-by-props-in-buffer)))

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



;;;###autoload
(defun org-roam-review ()
  (interactive)
  (let ((matches (org-roam-review--cache-collect
                  (-lambda (_ (value &as &plist :review))
                    (when review
                      (let ((date (org-read-date review)))
                        (when (org-time-less-p nil date)
                          value)))))))
    (pp-display-expression matches "*due notes*")))

;;;###autoload
(defun org-roam-review-pending ()
  (interactive)
  (let ((matches (org-roam-review--cache-collect
                  (-lambda (_ (value &as &plist :maturity :review))
                    (unless (and review maturity)
                      value)))))
    (pp-display-expression matches "*pending notes*")))



(defun org-roam-review--set-maturity (maturity)
  (cl-assert (member maturity org-roam-review-maturity-values))
  (org-with-wide-buffer
   (when (org-roam-dailies--daily-note-p)
     (user-error "Cannot set maturity on daily file"))
   (org-set-property "MATURITY" maturity)
   (message "Maturity set to '%s'" maturity)))

;;;###autoload
(defun org-roam-review-set-budding ()
  "Set the current note as a 'budding' note."
  (interactive)
  (org-roam-review--set-maturity "budding"))

;;;###autoload
(defun org-roam-review-set-seedling ()
  "Set the current note as a 'seedling' note."
  (interactive)
  (org-roam-review--set-maturity "seedling"))

;;;###autoload
(defun org-roam-review-set-evergreen ()
  "Set the current note as a 'evergreen' note."
  (interactive)
  (org-roam-review--set-maturity "evergreen"))




;;;###autoload
(define-minor-mode org-roam-review-mode
  "Minor mode to enable book-keeping used for notes reviews"
  :group 'org-roam-review
  (if org-roam-review-mode
      (add-hook 'after-save-hook #'org-roam-review--cache-update nil t)
    (remove-hook 'after-save-hook #'org-roam-review--cache-update t)))

(provide 'org-roam-review)

;;; org-roam-review.el ends here
