;;; ol-mac-app.el --- Link type for macOS applications  -*- lexical-binding: t; -*-

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

(require 'subr-x)
(require 'ol)

(defun org-link-complete-mac-app (&rest _)
  (cl-labels ((apps-in (dir)
                       (directory-files dir t (rx ".app" eos))))
    (let* ((choices (thread-last (append (apps-in "/Applications")
                                         (apps-in "/System/Applications")
                                         (apps-in "/System/Applications/Utilities")
                                         (apps-in "~/Applications"))
                                 (seq-map #'file-name-base)
                                 (seq-uniq)
                                 (seq-sort #'string<)))
           (choice (completing-read "Application:" choices)))
      (concat "mac-app:" choice))))

(defun org-link-follow-mac-app (app-name &rest _)
  (start-process "open" " open" "open" "-a" app-name))

(org-link-set-parameters "mac-app"
                         :follow #'org-link-follow-mac-app
                         :complete #'org-link-complete-mac-app)

(provide 'ol-mac-app)

;;; ol-mac-app.el ends here
