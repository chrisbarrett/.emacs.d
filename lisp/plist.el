;;; plist.el --- Utilities for working with plists  -*- lexical-binding: t; -*-

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

(defun plist-keys (plist)
  (seq-map #'car (-partition-all 2 plist)))

(defun plist--pred-name-for-type (type)
  (intern (format "%s-p" type)))

(defmacro plist-define-predicate (type required-keys)
  (cl-assert (symbolp type))
  (cl-assert (listp required-keys))
  (cl-assert (seq-every-p #'keywordp required-keys))
  `(defun ,(plist--pred-name-for-type type) (value)
     (when (listp value)
       (let ((required-keys ',required-keys))
         (let ((keys (plist-keys value)))
           (and (null (seq-difference required-keys keys))
                (seq-every-p (lambda (key)
                               (plist-get value key))
                             required-keys)))))))

(defun plist--pred-accessor-name (type keyword)
  (intern (format "%s-%s" type (string-remove-prefix ":" (symbol-name keyword)))))

(defmacro plist-define-getter (type key)
  (cl-assert (symbolp type))
  (cl-assert (keywordp key))
  (let ((pred-fn (plist--pred-name-for-type type)))
    `(defun ,(plist--pred-accessor-name type key) (,type)
       ,(format "Lookup `%s' in a plist of type `%s'." key type)
       (when (fboundp ',pred-fn)
         (cl-assert (,pred-fn ,type)))
       (plist-get ,type ,key))))

(defmacro plist-define-create (type)
  (cl-assert (symbolp type))
  `(defun ,(intern (format "%s-create" type)) (&rest attrs)
     (cl-assert (,(plist--pred-name-for-type type) attrs))
     attrs))

(cl-defmacro plist-define (type &key required optional)
  (declare (indent 1))
  (cl-assert (symbolp type))
  (cl-assert (listp required))
  (cl-assert (listp optional))
  `(progn
     (plist-define-predicate ,type ,required)
     (plist-define-create ,type)
     ,@(seq-map (lambda (it) `(plist-define-getter ,type ,it))
                (seq-union required optional))))

(provide 'plist)

;;; plist.el ends here
