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


;; Validator output representation

(defun schema-validation-success (value)
  (list :schema-validation-result value))

(defun schema-validation-failure (&optional reason)
  (list :schema-validation-error reason))

(defun schema-validation-result-p (value)
  (ignore-errors
    (or (schema-validation-success-p value)
        (schema-validation-failure-p value))))

(defun schema-validation-success-p (validated)
  (plist-member validated :schema-validation-result))

(defun schema-validation-failure-p (validated)
  (not (schema-validation-success-p validated)))

(defun schema-validation-get-result (validated)
  (plist-get validated :schema-validation-result))

(defun schema-validation-get-error (validated)
  (plist-get validated :schema-validation-error))


;; Validation result utilities

(defun schema-validation-map (f validated)
  "Apply a function to the value inside a validation result.

Notionally, this is a function of type `(a -> b) -> Result a -> Result b'.

F is a function `(a -> b)'.

VALIDATED is the output of a validator, where the payload is of
type `a'.

Returns a validated output with a payload of type `b'."
  (if (schema-validation-success-p validated)
      (schema-validation-success (funcall f (schema-validation-get-result validated)))
    validated))

(defun schema-validation-join (validated)
  "Collapse a nested validation result into a single result.

Notionally, this is a function of type `Result (Result a) -> Result a'.

VALIDATED is the output of a validator where the payload is a
nested validation result."
  (cond ((schema-validation-failure-p validated)
         validated)
        ((schema-validation-success-p validated)
         (schema-validation-get-result validated))))

(defun schema-validation-resolve (value)
  "Ensure that VALUE represents a schema validation result."
  (if (schema-validation-result-p value)
      value
    (schema-validation-success value)))



;; Schema primitives

(defun schema--check-equal (a b)
  (if (equal a b)
      (schema-validation-success a)
    (schema-validation-failure)))

(defun schema--check-apply-predicate (f x)
  (if (funcall f x)
      (schema-validation-success x)
    (schema-validation-failure)))

(defun schema--check-alternatives (predicates value)
  (or (seq-reduce (lambda (result pred)
                    (if result
                        result
                      (let ((attempt (funcall pred value)))
                        (when (schema-validation-success-p attempt)
                          attempt))))
                  predicates
                  nil)
      (schema-validation-failure)))

(defun schema--check-refinements (predicates value)
  (or
   (seq-reduce (lambda (continue pred)
                 (when continue
                   (let ((result (funcall pred value)))
                     (when (schema-validation-success-p result)
                       result))))
               predicates
               t)
   (schema-validation-failure)))

(defun schema--check-not (pred value)
  (let ((result (funcall pred value)))
    (if (schema-validation-failure-p result)
        (schema-validation-success value)
      (schema-validation-failure))))



;;;###autoload
(defun schema-compile (form)
  (pcase form
    ((or (pred numberp) (pred stringp) (pred keywordp) `(quote ,(pred symbolp)))
     `(lambda (value)
        (schema--check-equal ,form value)))

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

    ((or `(not) `(not ,_ , _ . ,_))
     (schema--raise-compilation-error form "`not' must have at single term"))
    (`(not ,form)
     (let ((pred (schema-compile form)))
       `(lambda (value)
          (schema--check-not ',pred value))))

    ((pred functionp)
     `(lambda (value)
        (schema--check-apply-predicate ',form value)))

    (_
     (schema--raise-compilation-error form "unrecognised syntax"))))

(defconst schema--default-docstring
  "VALUE is any Lisp value that will be checked against the schema.

Returns a schema validation result object, which should not be
interacted with directly. Instead, either:

- pass this function to `schema-validate', which returns the
  result on success or signals an error.

- call this validation function and use
  `schema-validation-get-result' on the return value, which
  returns the value on success or `nil' on failure.")


;;;###autoload
(defmacro schema-define (name schema &optional docstring)
  "Define NAME as a schema validation function.

SCHEMA is a schema DSL form.

DOCSTRING, if given, will prepended to the generated function's
docstring."
  (declare (indent defun) (doc-string 3))
  (let ((doc (concat docstring
                     (when docstring "\n\n")
                     schema--default-docstring)))

    `(cl-eval-when (compile load eval)
       (defalias ',name ,(schema-compile schema) ,doc))))

;;;###autoload
(defmacro schema (form)
  (schema-compile form))

;;;###autoload
(defun schema-validate (validator value)
  "Run a schema validator function on a value.

VALIDATOR is either a compiled schema defined by `schema-define',
or a schema DSL literal expressed with `schema'.

VALUE is any Lisp object.

Returns the output of the validator on success, or signals an
error if validations fails."
  (let ((result (funcall validator value)))
    (if (schema-validation-failure-p result)
        (schema--raise-validation-error validator value (schema-validation-get-error result))
      (schema-validation-get-result result))))

(provide 'schema)

;;; schema.el ends here
