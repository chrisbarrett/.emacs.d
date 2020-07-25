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

(require 'seq)

(define-error 'schema-compilation-error "Malformed schema form")

(define-error 'schema-validation-error "Value failed validation against schema")

(defsubst schema--raise-compilation-error (form reason)
  (signal 'schema-compilation-error (list :form form
                                    :reason reason)))

(defsubst schema--raise-validation-error (form value info)
  (signal 'schema-validation-error (list :form form
                                   :value value
                                   :info info)))


(defun schema--check-equal (a b)
  (if (equal a b)
      (list :value a)
    (list :errors t)))

(defun schema--check-apply-predicate (f x)
  (if (funcall f x)
      (list :value x)
    (list :errors t)))

(defun schema--check-alternatives (predicates value)
  (or (seq-reduce (lambda (result pred)
                    (if result
                        result
                      (let ((attempt (funcall pred value)))
                        (when (plist-member attempt :value)
                          attempt))))
                  predicates
                  nil)
      (list :errors t)))

(defun schema--check-refinements (predicates value)
  (or
   (seq-reduce (lambda (continue pred)
                 (when continue
                   (let ((result (funcall pred value)))
                     (when (plist-member result :value)
                       result))))
               predicates
               t)
   (list :errors t)))


;;;###autoload
(defun schema-compile (form)
  (pcase form
    ((or (pred numberp) (pred stringp))
     `(lambda (value)
        (schema--check-equal ,form value)))

    ((pred functionp)
     `(lambda (value)
        (schema--check-apply-predicate ',form value)))

    (`(and)
     (schema--raise-compilation-error form "`and' must have at least one term"))
    (`(and . ,forms)
     (let ((preds (seq-map #'schema-compile forms)))
       `(lambda (value)
          (schema--check-refinements ',preds value))))

    (`(or)
     (schema--raise-compilation-error form "`or' must have at least one term"))
    (`(or . ,forms)
     (let ((preds (seq-map #'schema-compile forms)))
       `(lambda (value)
          (schema--check-alternatives ',preds value))))
    (_
     (schema--raise-compilation-error form "unrecognised syntax"))))

;;;###autoload
(defmacro schema (form)
  (schema-compile form))

;;;###autoload
(defun schema-validate (s value)
  (let ((result (funcall s value)))
    (if (plist-member result :errors)
        (schema--raise-validation-error s value (plist-get result :errors))
      (plist-get result :value))))

(provide 'schema)

;;; schema.el ends here
