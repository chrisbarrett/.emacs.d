;;; seq-extras.el --- Extra sequence functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Chris Barrett

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
;;; Code:

(defun seq-max-by (comparator sequence)
  (seq-reduce (lambda (acc it)
                (if (funcall comparator it acc)
                    it
                  acc))
              sequence
              nil))

(defun seq-min-by (comparator sequence)
  (seq-reduce (lambda (acc it)
                (if (funcall comparator it acc)
                    acc
                  it))
              sequence
              nil))

(defun seq-shuffle (sequence)
  (seq-sort-by (lambda (_) (random)) #'<= sequence))

(provide 'seq-extras)

;;; seq-extras.el ends here
