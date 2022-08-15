;;; org-collapse-citations.el --- Hide citation bodies  -*- lexical-binding: t; -*-

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

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Hides citation bodies using overlays, unless cursor is positioned over them
;; for editing.

;;; Code:

(require 'org)
(require 'subr-x)

(defun org-collapse-citations--citation-ov-at (pt)
  (seq-find (lambda (ov) (equal 'org-pretty-citation (overlay-get ov 'type)))
            (overlays-at pt)))

(defun org-collapse-citations--activate-citation-ov (ov)
  (overlay-put ov 'priority 2000)
  (overlay-put ov 'evaporate t)
  (overlay-put ov 'invisible t)
  (overlay-put ov 'display "[@]")
  (overlay-put ov 'type 'org-pretty-citation))

(defun org-collapse-citations--deactivate-citation-ov (ov)
  (overlay-put ov 'invisible nil)
  (overlay-put ov 'display nil))

(defun org-collapse-citations--citation-cursor-sensor-function (_window prev-pt action)
  (pcase action
    ('entered
     (when-let* ((ov (org-collapse-citations--citation-ov-at (point))))
       (org-collapse-citations--deactivate-citation-ov ov)))
    ('left
     (when-let* ((ov (org-collapse-citations--citation-ov-at prev-pt)))
       (org-collapse-citations--activate-citation-ov ov)))))

(defun org-collapse-citations-activation-function (citation)
  (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
    (unless (or (org-in-regexp (rx bol ":" (* nonl)))
                (org-collapse-citations--citation-ov-at beg))
      (let ((ov (make-overlay beg end nil t)))
        (overlay-put ov 'cursor-sensor-functions '(org-collapse-citations--citation-cursor-sensor-function))
        (org-collapse-citations--activate-citation-ov ov)))))


(provide 'org-collapse-citations)

;;; org-collapse-citations.el ends here
