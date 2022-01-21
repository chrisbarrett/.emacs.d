;;; doi.el --- Utils for retrieving a DOI  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'dash)
(require 'plist)
(require 'ts)

(defvar doi-base-url "https://doi.org/")

(defvar url-http-end-of-headers nil)

(plist-define doi
  :required (:doi :url :title :type :published :authors)
  :optional (:isbn :publisher :edition-number))

(plist-define doi-author
  :required (:given :family :sequence))

(defun doi--downcase-symbol (sym)
  (intern (downcase (symbol-name sym))))

(defun doi-parse-from-json (json)
  (-let* (((json &as &plist :isbn [isbn] :author :published) (plist-map-keys #'doi--downcase-symbol json))
          ((&plist :date-parts [[year month day]]) published)
          (published (ts-parse (format "%s-%s-%s" year month day))))
    (apply 'doi-create (append (list :authors (seq-map (-applify #'doi-author-create) author)
                                     :isbn isbn
                                     :published published)
                               json))))

(defun doi-retrieve (url)
  (let ((url-request-method "GET")
        (url-mime-accept-string "application/citeproc+json"))
    (with-current-buffer (url-retrieve-synchronously (concat doi-base-url url))
      (let ((json (json-parse-string (buffer-substring url-http-end-of-headers (point-max))
                                     :object-type 'plist)))
        (doi-parse-from-json json)))))

(provide 'doi)

;;; doi.el ends here
