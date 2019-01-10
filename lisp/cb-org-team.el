;;; cb-org-team.el --- Utilities for working with teams -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'org)
(require 'org-capture)
(require 'subr-x)

(defvar cb-org-team-members nil
  "List of team members to log for.")

(defvar cb-org-team-name nil
  "Name of the team to log for.")

(defun cb-org-team--goto-toplevel-heading (name)
  (cl-labels ((loop ()
                    (cond
                     ((equal name (plist-get (cadr (org-element-at-point)) :title))
                      (point))
                     ((org-get-next-sibling)
                      (loop)))))
    (when-let* ((pos (save-excursion
                       (goto-char (point-min))
                       (outline-next-heading)
                       (loop))))
      (goto-char pos))))

(defun cb-org-team--goto-subheading (name)
  (cl-labels ((loop ()
                    (cond
                     ((equal name (plist-get (cadr (org-element-at-point)) :title))
                      (point))
                     ((org-get-next-sibling)
                      (loop)))))
    (when-let* ((pos (save-excursion
                       (org-goto-first-child)
                       (loop))))
      (goto-char pos))))

(defun cb-org-team--ensure-children-exist (path-strings)
  (dolist (path path-strings)
    (unless (cb-org-team--goto-subheading path)
      (goto-char (line-end-position))
      (org-insert-subheading nil)
      (insert (format "%s\n" path)))))

(defun cb-org-team-log-heading ()
  (cl-assert cb-org-team-members t)
  (cl-assert cb-org-team-name t)
  (-let* ((team-member (completing-read "Team member: " cb-org-team-members nil t))
          (date-headline (format-time-string "%Y Week %U"))
          ((headings &as top . children) (list cb-org-team-name date-headline team-member)))
    (save-restriction
      (widen)
      (cb-org-team--goto-toplevel-heading top)
      (cb-org-team--ensure-children-exist children)
      (point))))


(provide 'cb-org-team)

;;; cb-org-team.el ends here
