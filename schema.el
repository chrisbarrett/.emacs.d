;;; schema.el --- Data validation using schemas. -*- lexical-binding: t; -*-

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

(define-error 'validation-error "Value failed validation against schema")

(defun schema-compile (form)
  (pcase form
    ((or (pred numberp) (pred stringp))
     `(lambda (value)
        (if (equal ,form value)
            (list :value value)
          (list :errors t))))
    (`(or ,l ,r)
     (let ((l-comp (schema-compile l))
           (r-comp (schema-compile r)))
       `(lambda (value)
          (let ((attempt (funcall ,l-comp value)))
            (if (plist-member attempt :value)
                attempt
              (let ((attempt2 (funcall ,r-comp value)))
                (if (plist-member attempt2 :value)
                    attempt2
                  (list :errors t))))))))
    (_
     (error "Malformed schema term: %s" form))))

(defmacro schema (form)
  (schema-compile form))

(defun schema-validate (s x)
  (let ((result (funcall s x)))
    (if (plist-member result :errors)
        (signal 'validation-error (list :schema s :value x))
      (plist-get result :value))))

(provide 'schema)

;;; schema.el ends here
