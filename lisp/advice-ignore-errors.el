;;; advice-ignore-errors.el --- Utility function for ignoring errors  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Chris Barrett

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

(defun advice-ignore-errors (f &rest args)
  (ignore-errors
    (apply f args)))

(provide 'advice-ignore-errors)

;;; advice-ignore-errors.el ends here
