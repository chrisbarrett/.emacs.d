;;; ol-ticker.el --- Stock ticker links  -*- lexical-binding: t; -*-

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

(require 'ol)
(require 'thingatpt)

(defconst org-link-stock-ticker-rx
  (rx (? "^") (** 1 5 alpha) (? "." (** 1 2 alpha))))

(defun org-link-follow-ticker (ticker &rest _)
  (let ((query (url-build-query-string `((t ,ticker)))))
    (browse-url (concat "https://finviz.com/quote.ashx?" query))))

(defun org-link-complete-ticker ()
  (save-match-data
    (if (thing-at-point-looking-at org-link-stock-ticker-rx)
        (concat "ticker:" (upcase (match-string 0)))
      "ticker:")))

(org-link-set-parameters "ticker"
                         :complete #'org-link-complete-ticker
                         :follow #'org-link-follow-ticker)

(provide 'ol-ticker)

;;; ol-ticker.el ends here
