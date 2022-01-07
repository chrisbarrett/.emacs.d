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

(defun org-roam-review ()
  "Find a note that's ready for review"
  (interactive)
  (message "No more notes to review"))

(defun org-roam-review-set-budding ()
  "Set the current note as a 'budding' note."
  (interactive)
  (message "Note type set to 'budding'"))

(defun org-roam-review-set-seedling ()
  "Set the current note as a 'seedling' note."
  (interactive)
  (message "Note type set to 'seedling'"))

(defun org-roam-review-set-evergreen ()
  "Set the current note as a 'evergreen' note."
  (interactive)
  (message "Note type set to 'evergreen'"))

(provide 'org-roam-review)

;;; org-roam-review.el ends here
