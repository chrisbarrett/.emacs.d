;;; cb-ensured-packages.el --- Write ensured packages to a JSON file for use by Nix. -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Barrett

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

(require 'json)

(defconst cb-ensured-packages-file (expand-file-name "ensured-packages.json" user-emacs-directory))

(defvar cb-ensured-packages nil)

(defun cb-ensured-packages-record (package &rest _)
  (add-to-list 'cb-ensured-packages package)
  t)

(defun cb-ensured-packages-write ()
  "Write the list of external packages ensured by this Emacs configuration.

This list is used by Nix to determine which packages to install."
  (interactive)
  (let ((packages (seq-sort #'string< cb-ensured-packages)))
    (with-temp-buffer
      (insert (json-encode (seq-into packages 'vector)))
      (json-pretty-print-buffer)
      (write-region (point-min) (point-max) cb-ensured-packages-file))))

(provide 'cb-ensured-packages)

;;; cb-ensured-packages.el ends here
