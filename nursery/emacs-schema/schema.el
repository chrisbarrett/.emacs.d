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

(require 'cl-lib)
(require 'seq)

(eval-and-compile
  (define-error 'schema-compilation-error "Malformed schema form")

  (define-error 'schema-validation-error "Value failed validation against schema")

  (defsubst schema--raise-compilation-error (form reason)
    (signal 'schema-compilation-error (list :reason reason
                                      :form form)))

  (defsubst schema--raise-validation-error (form value info)
    (signal 'schema-validation-error (list :form form
                                     :value value
                                     :info info))))


;; Validator output representation

(defun schema-validation-success (value)
  (list :schema-validation-result value))

(defun schema-validation-failure (&optional reason)
  (list :schema-validation-error reason))

(defun schema-validation-result-p (value)
  (ignore-errors
    (or (plist-member value :schema-validation-result)
        (plist-member value :schema-validation-error))))

(defun schema-validation-success-p (validated)
  (cl-assert (schema-validation-result-p validated) t)
  (plist-member validated :schema-validation-result))

(defun schema-validation-failure-p (validated)
  (cl-assert (schema-validation-result-p validated) t)
  (not (schema-validation-success-p validated)))

(defun schema-validation-get-result (validated)
  (cl-assert (schema-validation-result-p validated) t)
  (plist-get validated :schema-validation-result))

(defun schema-validation-get-error (validated)
  (cl-assert (schema-validation-result-p validated) t)
  (plist-get validated :schema-validation-error))


;; Validation result utilities

(defun schema-validation-map (f validated)
  "Apply a function to the value inside a validation result.

----------------------------------------
Notionally, this is a function of type:

  (a -> b) -> Result a -> Result b
----------------------------------------

F is a function `(a -> b)'.

VALIDATED is the output of a validator, where the payload is of
type `a'.

Returns a validated output with a payload of type `b'."
  (if (schema-validation-success-p validated)
      (schema-validation-success (funcall f (schema-validation-get-result validated)))
    validated))

(defun schema-validation-join (validated)
  "Collapse a nested validation result into a single result.

----------------------------------------
Notionally, this is a function of type:

  Result (Result a) -> Result a
----------------------------------------

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

(defun schema-validation-ap (left right)
  "Apply another validator function if the first succeeded.

----------------------------------------
Notionally, this is a function of type:

  Result a -> Result b -> Result b
----------------------------------------

LEFT and RIGHT are the output of validators.

If LEFT fails, return LEFT. Otherwise return RIGHT."
  (if (schema-validation-success-p left)
      right
    left))

(defun schema-validation-traverse* (validator validated-values)
  "Apply a validation function to a sequence of previously validated results.

----------------------------------------
Notionally, this is a function of type:

  (a -> Result b) -> [Result a] -> Result _
----------------------------------------

VALIDATOR is a function returning a validation result for each
element in the sequence.

VALIDATED-VALUES is a sequence of validation results.

The result is either failure, or a success with a meaningless
payload."
  (let ((unit (schema-validation-success t)))
    (schema-validation-ap
     (seq-reduce (lambda (acc value)
                   (if (schema-validation-success-p acc)
                       (funcall validator value)
                     (schema-validation-failure)))
                 validated-values
                 unit)
     unit)))


;; Schema primitives

(defun schema--equal (a b)
  (if (equal a b)
      (schema-validation-success a)
    (schema-validation-failure)))

(defun schema--funcall (f x)
  (let ((result (funcall f x)))
    (cond
     ((schema-validation-result-p result)
      result)
     (result
      (schema-validation-success x))
     (t
      (schema-validation-failure)))))

(defun schema--or (predicates value)
  (or (seq-reduce (lambda (result pred)
                    (if result
                        result
                      (let ((attempt (funcall pred value)))
                        (when (schema-validation-success-p attempt)
                          attempt))))
                  predicates
                  nil)
      (schema-validation-failure)))

(defun schema--and (predicates value)
  (let ((all-satisfied-p
         (seq-reduce (lambda (continue pred)
                       (when continue
                         (let ((result (funcall pred value)))
                           (when (schema-validation-success-p result)
                             result))))
                     predicates
                     t)))
    (if all-satisfied-p
        (schema-validation-success value)
      (schema-validation-failure))))

(defun schema--not (pred value)
  (let ((result (funcall pred value)))
    (if (schema-validation-failure-p result)
        (schema-validation-success value)
      (schema-validation-failure))))

(defun schema--positional (validators values)
  (cond
   ((not (sequencep values))
    (schema-validation-failure))
   ((equal (length validators) (length values))
    (let ((results
           (seq-map-indexed (lambda (value index)
                              (let ((validator (seq-elt validators index)))
                                (funcall validator value)))
                            values)))

      (schema-validation-ap
       (schema-validation-traverse* #'identity results)
       (schema-validation-success values))))
   (t
    (schema-validation-failure))))



;; Compiler

(eval-and-compile
  (defun schema--custom-pattern-ident (name)
    (intern (format "schema---%s-validator" name)))

  (defun schema-compile (form)
    "Compile a schema into a validator function.

FORM is a schema DSL form. See the documentation for `schema' for
an overview.

Returns a function that can be used with `schema-validate'."
    (pcase form
      ('_
       `(lambda (value)
          (schema-validation-success value)))

      ((or (pred numberp) (pred stringp) (pred keywordp) `(quote ,(pred symbolp)))
       `(lambda (value)
          (schema--equal ,form value)))

      (`(and)
       (schema--raise-compilation-error form "`and' must have at least one term"))
      (`(and . ,forms)
       (let ((preds (seq-map #'schema-compile forms)))
         `(lambda (value)
            (schema--and ',preds value))))

      (`(or)
       (schema--raise-compilation-error form "`or' must have at least one term"))
      (`(or . ,forms)
       (let ((preds (seq-map #'schema-compile forms)))
         `(lambda (value)
            (schema--or ',preds value))))

      ((and `(not . ,forms) (guard (/= 1 (length forms))))
       (schema--raise-compilation-error form (format "`not' must have at single term, but had %s terms"
                                               (length forms))))
      (`(not ,form)
       (let ((pred (schema-compile form)))
         `(lambda (value)
            (schema--not ',pred value))))

      ((and `(,sym . ,args)
            (guard (and (symbolp sym)
                        (fboundp (schema--custom-pattern-ident sym)))))
       (let ((pred (apply (schema--custom-pattern-ident sym)
                          args)))

         `(lambda (value)
            (schema--funcall ',pred value))))

      ((pred functionp)
       `(lambda (value)
          (schema--funcall ',form value)))

      ((pred vectorp)
       (let ((validators (seq-map #'schema-compile form)))
         `(lambda (value)
            (schema--positional ',validators value))))

      (_
       (schema--raise-compilation-error form "Bad form passed to `schema'")))))

(eval-and-compile
  (defconst schema--default-docstring
    "VALUE is any Lisp value that will be checked against the schema.

Returns a schema validation result object, which should not be
interacted with directly. Instead, either:

- pass this function to `schema-validate', which returns the
  result on success or signals an error.

- call this validation function and use
  `schema-validation-get-result' on the return value, which
  returns the value on success or `nil' on failure."))

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

    `(eval-when-compile
       (defalias ',name ,(schema-compile schema) ,doc))))

(defconst schema--default-pattern-docstring
  "Returns a validation function where the parameters in ARGLIST are bound.")

;;;###autoload
(defmacro schema-define-pattern (name arglist &rest body)
  "Define NAME as pattern for use in the schema validation language.

ARGLIST is the list of arguments expected by the pattern.

BODY is expected to return a validation function."
  (declare (indent defun) (doc-string 3))
  `(eval-when-compile
     (defun ,(schema--custom-pattern-ident name) ,arglist
       (schema-compile (progn ,@body)))))


(eval-when-compile
  (defconst schema--schema-docstring
    (with-temp-buffer
      (insert-file-contents (expand-file-name "schema-docstring.txt"
                                              (when load-file-name
                                                (file-name-directory load-file-name))))
      (goto-char (point-min))
      (forward-line 1)
      (buffer-substring (line-beginning-position) (point-max)))))

;;;###autoload
(eval `(defmacro schema (&rest form)
         ,(concat schema--schema-docstring "\n\n\(fn FORM)")

         (cl-assert (equal 1 (length form))
                    nil
                    (format "`schema' expects a single argument but got: %s" (cons 'schema form)))
         (schema-compile (car form))))


;;;###autoload
(defun schema-validate (validator value)
  "Run a schema validator function on a value.

See the documentation for the macro `schema' for usage examples.

VALIDATOR is typically built using the `schema' macro, but it can
be any function returning either a `schema-validation-success' or
`schema-validation-failure'.

VALUE is any Lisp object.

Returns the output of the validator on success, or signals an
error if validations fails."
  (let ((result (funcall validator value)))
    (if (schema-validation-failure-p result)
        (schema--raise-validation-error validator value (schema-validation-get-error result))
      (schema-validation-get-result result))))


;; Provided validators

(schema-define-pattern cons (car-type cdr-type)
  `(and consp
        (lambda (it)
          (schema-validation-ap
           (funcall (schema ,car-type) (car it))
           (funcall (schema ,cdr-type) (cdr it))))))

(schema-define-pattern seq (value-type)
  `(and sequencep
        (lambda (it)
          (schema-validation-traverse* (schema ,value-type) it))))

(schema-define-pattern list (value-type)
  `(and listp (seq ,value-type)))

(schema-define-pattern vector (value-type)
  `(and vectorp (seq ,value-type)))

(schema-define-pattern alist (key-type value-type)
  `(list (cons ,key-type ,value-type)))

(schema-define-pattern plist (key-type value-type)
  `(and listp
        (lambda (it) (cl-evenp (length it)))
        (lambda (values)
          (let ((pairs (seq-partition values 2)))
            (schema-validation-traverse* (schema [,key-type ,value-type]) pairs)))))

(schema-define-pattern hash (key-type value-type)
  `(and hash-table-p
        (lambda (hash)
          (schema-validation-traverse*
           (schema (cons ,key-type ,value-type))
           (let (kvps)
             (maphash (lambda (key value)
                        (push (cons key value) kvps))
                      hash)
             kvps)))))

(schema-define-pattern map (key-type value-type)
  `(or (alist ,key-type ,value-type)
       (plist ,key-type ,value-type)
       (hash ,key-type ,value-type)))

(schema-define-pattern optional (type)
  `(or null ,type))

(defun schema--syntax-check-num (plist)
  (cond
   ((< 1 (length (seq-intersection plist '(:lt :lte :max))))
    (schema--raise-compilation-error plist "Can provide only one of :max, :lt, :lte"))

   ((< 1 (length (seq-intersection plist '(:gt :gte :min))))
    (schema--raise-compilation-error plist "Can provide only one of :min, :gt, :gte"))

   (t
    (let ((lower (or (plist-get plist :min)
                     (plist-get plist :gte)
                     (when-let* ((n (plist-get plist :gt)))
                       (1+ n))
                     -1.0e+INF))

          (upper (or (plist-get plist :max)
                     (plist-get plist :lte)
                     (when-let* ((n (plist-get plist :lt)))
                       (1- n))
                     +1.0e+INF)))
      (unless (<= lower upper)
        (schema--raise-compilation-error plist "lower bound must be <= upper bound")))
    plist)))

(defun schema--eval-num (plist n)
  (and (if-let* ((incl-min (or (plist-get plist :min)
                               (plist-get plist :gte))))
           (<= incl-min n)
         t)
       (if-let* ((excl-min (plist-get plist :gt)))
           (< excl-min n)
         t)
       (if-let* ((incl-max (or (plist-get plist :max)
                               (plist-get plist :lte))))
           (<= n incl-max)
         t)
       (if-let* ((excl-max (plist-get plist :lt)))
           (< n excl-max)
         t)))

(schema-define-pattern num (&rest plist)
  `(and numberp
        (lambda (n)
          (schema--eval-num ',(schema--syntax-check-num plist) n))))

(schema-define-pattern nat (&rest plist)
  `(and wholenump
        (lambda (n)
          (schema--eval-num ',plist n))))

(schema-define port-number
  (nat :max 65535))

(defun schema--syntax-check-string (plist)
  (when-let* ((regexp (plist-get plist :match)))
    (unless (or (stringp regexp) (listp regexp))
      (schema--raise-compilation-error plist "Regexp must be a string or rx form"))

    (when (listp regexp)
      (condition-case err
          (rx-to-string regexp)
        (error
         (schema--raise-compilation-error plist (error-message-string err))))))

  plist)

(defun schema--eval-string (plist s)
  (if-let* ((regexp (plist-get plist :match)))
      (string-match-p (if (stringp regexp) regexp (rx-to-string regexp))
                      s)
    t))


(schema-define-pattern string (&rest plist)
  `(and stringp
        (lambda (s)
          (schema--eval-string ',(schema--syntax-check-string plist) s))))

(provide 'schema)

;;; schema.el ends here
